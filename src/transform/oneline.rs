use crate::python::parse;
use num_bigint::BigInt;
use rustpython_parser::ast::{
    BooleanOperator, Comparison, Comprehension, ComprehensionKind, ConversionFlag, ExpressionType,
    ImportSymbol, Keyword, Located, Location, Number, Operator, Parameter, Parameters, Program,
    StatementType, StringGroup, UnaryOperator, Varargs,
};
use std::collections::{HashSet, VecDeque};

use super::super::python::serialize_expression;

const PREP_KEY: &str = "__INL__";
const CORE_NAME: &str = "__INL__CORE";
const COND_NAME: &str = "__INL__COND";
const TEMP_NAME: &str = "__INL__TEMP";
const SPLIT_NAME: &str = "__INL__SPLIT";
const STATE_NAME: &str = "__INL__STATE";
const IMPORT_KEY: &str = "__INL__IMPORT_";
const VARS_NAME: &str = "vars";
const HASATTR_NAME: &str = "hasattr";
const GLOBALS_NAME: &str = "globals";
const UPDATE_NAME: &str = "update";
const SLICE_NAME: &str = "slice";
const EMPTY_NAME: &str = "_";
const THROW_NAME: &str = "throw";
const SYSTEM_NAME: &str = "sys";
const CTYPE_NAME: &str = "ctypes";
const INSPECT_NAME: &str = "inspect";
const EXCEPTHOOK_NAME: &str = "excepthook";
const BUILTINS_NAME: &str = "__builtins__";
const SETATTR_NAME: &str = "setattr";
const DELATTR_NAME: &str = "delattr";
const SETITEM_NAME: &str = "__setitem__";
const DELITEM_NAME: &str = "__delitem__";
const TEMP_EXCEPTHOOK: &str = "__INL__EXCEPTHOOK";
const EXCEPTION_DEF_TYPE: &str = "RuntimeError";
const EXCEPTION_DEF_MSG: &str = "No active exception to reraise";

/*
* Manual state handling:
*
* we assume that all code in a branch after a return, pass, or continue statement can be eliminated.
* we also assume that if a return statement happens in a conditional statement, all remaining code will execute in the else block.
*
* as such, we can handle the execution flow in the state variable
* [0] Return value buffer
* [1] Target indentation
*
* We can then define this state in each function and add checks in conditionals to iterate towards the target indentation.
* (i): this may add extra code and checks, which could potentially affect program performance, but it's the only effective solution I've come up with.
*/

pub fn oneline(ast: Program) -> Program {
    let mut local = Program { statements: vec![] };

    let imports = extract_imports(&ast.statements);
    let imports_prepended_inlined = imports
        .iter()
        .map(|import| match &import.node {
            StatementType::Import { names } => {
                format!(
                    "import {}",
                    &names
                        .iter()
                        .map(|name| match &name.alias {
                            Some(alias) => format!("{} as {}{}", name.symbol, IMPORT_KEY, alias),
                            None => format!("{} as {}{}", name.symbol, IMPORT_KEY, name.symbol),
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            StatementType::ImportFrom {
                level: _,
                module,
                names,
            } => {
                let mut import = "".to_string();

                if let Some(module) = module {
                    import += &format!("from {} ", module)
                }

                import += &format!(
                    "import {}",
                    &names
                        .iter()
                        .map(|name| match &name.alias {
                            Some(alias) => format!("{} as {}{}", name.symbol, IMPORT_KEY, alias),
                            None => format!("{} as {}{}", name.symbol, IMPORT_KEY, name.symbol),
                        })
                        .collect::<HashSet<String>>()
                        .into_iter()
                        .collect::<Vec<String>>()
                        .join(", ")
                );

                import
            }
            _ => panic!("Extracted import cannot contain another statement type"),
        })
        .collect::<Vec<String>>()
        .join(";");

    local
        .statements
        .push(to_located(to_statement(ExpressionType::Identifier {
            name: if imports_prepended_inlined.len() > 0 {
                imports_prepended_inlined + ";"
            } else {
                imports_prepended_inlined
            },
        })));

    let mut statements_list = vec![to_located(reset_state())];

    for statement in prepare_body(&ast.statements, 0, 0) {
        statements_list.push(to_located(clone_expression(&statement.node)));
    }

    local
        .statements
        .push(to_located(to_statement(ExpressionType::List {
            elements: statements_list,
        })));

    return local;
}

fn to_internal(s: String) -> String {
    format!("{}{}", PREP_KEY, s)
}

fn reset_state() -> ExpressionType {
    update_state(ExpressionType::None, 1)
}

fn update_state(value: ExpressionType, target: u32) -> ExpressionType {
    ExpressionType::NamedExpression {
        left: Box::from(to_located(ExpressionType::Identifier {
            name: STATE_NAME.to_string(),
        })),
        right: Box::from(to_located(ExpressionType::Tuple {
            elements: vec![
                to_located(value),
                to_located(ExpressionType::Number {
                    value: Number::Integer {
                        value: BigInt::from(target),
                    },
                }),
            ],
        })),
    }
}

fn state_target() -> ExpressionType {
    ExpressionType::Subscript {
        a: Box::from(to_located(ExpressionType::Identifier {
            name: STATE_NAME.to_string(),
        })),
        b: Box::from(to_located(ExpressionType::Number {
            value: Number::Integer {
                value: BigInt::from(1),
            },
        })),
    }
}

fn check_state_target(target: u32) -> ExpressionType {
    ExpressionType::Compare {
        vals: vec![
            to_located(state_target()),
            to_located(ExpressionType::Number {
                value: Number::Integer {
                    value: BigInt::from(target),
                },
            }),
        ],
        ops: vec![Comparison::Greater],
    }
}

fn optional_variable(identifier: String) -> ExpressionType {
    ExpressionType::IfExpression {
        test: Box::from(to_located(ExpressionType::Compare {
            vals: vec![
                to_located(ExpressionType::String {
                    value: StringGroup::Constant {
                        value: identifier.to_string(),
                    },
                }),
                to_located(ExpressionType::Call {
                    function: Box::from(to_located(ExpressionType::Identifier {
                        name: VARS_NAME.to_string(),
                    })),
                    args: vec![],
                    keywords: vec![],
                }),
            ],
            ops: vec![Comparison::In],
        })),
        body: Box::from(to_located(ExpressionType::Identifier {
            name: identifier.to_string(),
        })),
        orelse: Box::from(to_located(ExpressionType::IfExpression {
            test: Box::from(to_located(ExpressionType::Call {
                function: Box::from(to_located(ExpressionType::Identifier {
                    name: HASATTR_NAME.to_string(),
                })),
                args: vec![
                    to_located(ExpressionType::Identifier {
                        name: BUILTINS_NAME.to_string(),
                    }),
                    to_located(ExpressionType::String {
                        value: StringGroup::Constant {
                            value: identifier.to_string(),
                        },
                    }),
                ],
                keywords: vec![],
            })),
            body: Box::from(to_located(ExpressionType::Attribute {
                value: Box::from(to_located(ExpressionType::Identifier {
                    name: BUILTINS_NAME.to_string(),
                })),
                name: identifier.to_string(),
            })),
            orelse: Box::from(to_located(ExpressionType::None)),
        })),
    }
}

fn to_statement(expression: ExpressionType) -> StatementType {
    return StatementType::Expression {
        expression: to_located(expression),
    };
}

// We move each import at the top of the program, they will stay as Statements
// and will be inlined with ";" separators. To keep the potential scoping of the import,
// we will prepend the moved global import with an identifier, and re-declare their
// true identifier with the prepended version at the time they are truly imported.
fn extract_imports(body: &Vec<Located<StatementType>>) -> Vec<Located<StatementType>> {
    let mut imports = vec![];
    for statement in body {
        match &statement.node {
            StatementType::Import { names } => imports.push(to_located(StatementType::Import {
                names: names
                    .iter()
                    .map(|i| ImportSymbol {
                        symbol: i.symbol.to_string(),
                        alias: match &i.alias {
                            Some(alias) => Some(alias.to_string()),
                            None => None,
                        },
                    })
                    .collect::<Vec<ImportSymbol>>(),
            })),
            StatementType::ImportFrom {
                level,
                module,
                names,
            } => imports.push(to_located(StatementType::ImportFrom {
                level: level.clone(),
                module: match module {
                    Some(module) => Some(module.to_string()),
                    None => None,
                },
                names: names
                    .iter()
                    .map(|i| ImportSymbol {
                        symbol: i.symbol.to_string(),
                        alias: match &i.alias {
                            Some(alias) => Some(alias.to_string()),
                            None => None,
                        },
                    })
                    .collect::<Vec<ImportSymbol>>(),
            })),
            StatementType::If {
                test: _,
                body,
                orelse,
            } => {
                imports.append(&mut extract_imports(body));
                if let Some(orelse) = &orelse {
                    imports.append(&mut extract_imports(orelse));
                }
            }
            StatementType::For {
                is_async: _,
                target: _,
                iter: _,
                body,
                orelse,
            } => {
                imports.append(&mut extract_imports(body));
                if let Some(orelse) = &orelse {
                    imports.append(&mut extract_imports(orelse));
                }
            }
            StatementType::While {
                test: _,
                body,
                orelse,
            } => {
                imports.append(&mut extract_imports(body));
                if let Some(orelse) = &orelse {
                    imports.append(&mut extract_imports(orelse));
                }
            }
            StatementType::ClassDef {
                name: _,
                body,
                bases: _,
                keywords: _,
                decorator_list: _,
            } => {
                imports.append(&mut extract_imports(body));
            }
            StatementType::With {
                is_async: _,
                items: _,
                body,
            } => {
                imports.append(&mut extract_imports(body));
            }
            StatementType::FunctionDef {
                is_async: _,
                name: _,
                args: _,
                body,
                decorator_list: _,
                returns: _,
            } => {
                imports.append(&mut extract_imports(body));
            }
            StatementType::Try {
                body,
                handlers,
                orelse,
                finalbody,
            } => {
                // We need to import the sys library for using the excepthook
                imports.push(to_located(StatementType::Import {
                    names: vec![ImportSymbol {
                        symbol: SYSTEM_NAME.to_string(),
                        alias: None,
                    }],
                }));

                imports.append(&mut extract_imports(body));

                for handler in handlers {
                    imports.append(&mut extract_imports(&handler.body));
                }

                if let Some(orelse) = &orelse {
                    imports.append(&mut extract_imports(orelse));
                }

                if let Some(finalbody) = &finalbody {
                    imports.append(&mut extract_imports(finalbody));
                }
            }
            StatementType::Delete { targets } => {
                for target in targets {
                    match &target.node {
                        ExpressionType::Subscript { a: _, b: _ } => {}
                        _ => {
                            imports.push(to_located(StatementType::Import {
                                names: vec![ImportSymbol {
                                    symbol: INSPECT_NAME.to_string(),
                                    alias: None,
                                }],
                            }));

                            imports.push(to_located(StatementType::Import {
                                names: vec![ImportSymbol {
                                    symbol: CTYPE_NAME.to_string(),
                                    alias: None,
                                }],
                            }));
                        }
                    }
                }
            }
            _ => {}
        }
    }

    imports
}

fn prepare_body(
    body: &Vec<Located<StatementType>>,
    level: u32,
    from: usize,
) -> Vec<Located<ExpressionType>> {
    let mut local_statements = VecDeque::new();
    let mut globals: HashSet<String> = HashSet::new();

    for i in from..body.len() {
        let statement = &body[i];

        // Extract global to the outer border of the body
        if let StatementType::Global { names } = &statement.node {
            globals.extend(names.clone());
            continue;
        }

        // Convert current expression
        local_statements.push_back(to_located(to_expression(&statement.node, level + 1)));

        // Remove each statements after a return, break or continue is found
        // We can give the indent of the target (if: 0, while: 1, for: 1, otherwise nothing)
        let split_flow: u32 = match &statement.node {
            StatementType::Break => break,
            StatementType::Continue => break,
            StatementType::Return { .. } => break,
            StatementType::Pass => break,
            StatementType::If { .. } => 1,
            StatementType::While { .. } => 2,
            StatementType::For { .. } => 2,
            _ => 0,
        };

        // Add a check on the state after each for/while loop and if statements
        if split_flow > 0 {
            let flow_target = level + split_flow - 1; // to account for split_flow that start at 1
            
            let check_state = ExpressionType::IfExpression {
                test: Box::from(to_located(check_state_target(level))),
                body: Box::from(to_located(ExpressionType::List {
                    elements: prepare_body(body, flow_target, i + 1),
                })),
                orelse: Box::from(to_located(ExpressionType::None)),
            };

            local_statements.push_back(to_located(check_state));

            break;
        }
    }

    // move globals declaration to begining and update at the end
    for global in globals {
        local_statements.push_front(global_declaration(global.to_string()));
        local_statements.push_back(global_update(global.to_string()));
    }

    local_statements.into_iter().collect()
}

// x := globals()["x"]
fn global_declaration(global: String) -> Located<ExpressionType> {
    to_located(ExpressionType::NamedExpression {
        left: Box::from(to_located(ExpressionType::Identifier {
            name: global.to_string(),
        })),
        right: Box::from(to_located(ExpressionType::Subscript {
            a: Box::from(to_located(ExpressionType::Call {
                function: Box::from(to_located(ExpressionType::Identifier {
                    name: GLOBALS_NAME.to_string(),
                })),
                args: vec![],
                keywords: vec![],
            })),
            b: Box::from(to_located(ExpressionType::String {
                value: StringGroup::Constant {
                    value: global.to_string(),
                },
            })),
        })),
    })
}

// globals().update({'x': x})
fn global_update(global: String) -> Located<ExpressionType> {
    to_located(ExpressionType::Call {
        function: Box::from(to_located(ExpressionType::Attribute {
            value: Box::from(to_located(ExpressionType::Call {
                function: Box::from(to_located(ExpressionType::Identifier {
                    name: GLOBALS_NAME.to_string(),
                })),
                args: vec![],
                keywords: vec![],
            })),
            name: UPDATE_NAME.to_string(),
        })),
        args: vec![to_located(ExpressionType::Dict {
            elements: vec![(
                Some(to_located(ExpressionType::String {
                    value: StringGroup::Constant {
                        value: global.to_string(),
                    },
                })),
                to_located(ExpressionType::Identifier {
                    name: global.to_string(),
                }),
            )],
        })],
        keywords: vec![],
    })
}

fn inline_assign_statement(
    target: &Located<ExpressionType>,
    value: &Located<ExpressionType>,
) -> ExpressionType {
    match &target.node {
        ExpressionType::Subscript { a, b } => ExpressionType::Call {
            function: Box::from(to_located(ExpressionType::Attribute {
                value: Box::from(to_located(clone_expression(&a.node))),
                name: SETITEM_NAME.to_string(),
            })),
            args: vec![
                to_located(clone_expression(&b.node)),
                to_located(clone_expression(&value.node)),
            ],
            keywords: vec![],
        },
        ExpressionType::Attribute {
            value: attr_target,
            name: attr_name,
        } => ExpressionType::Call {
            function: Box::from(to_located(ExpressionType::Identifier {
                name: SETATTR_NAME.to_string(),
            })),
            args: vec![
                to_located(clone_expression(&attr_target.node)),
                to_located(ExpressionType::String {
                    value: StringGroup::Constant {
                        value: attr_name.to_string(),
                    },
                }),
                to_located(clone_expression(&value.node)),
            ],
            keywords: vec![],
        },
        ExpressionType::List { elements } => inline_assign_list(elements, value),
        ExpressionType::Tuple { elements } => inline_assign_list(elements, value),
        _ => ExpressionType::NamedExpression {
            left: Box::from(to_located(clone_expression(&target.node))),
            right: Box::from(to_located(clone_expression(&value.node))),
        },
    }
}

fn inline_assign_list(
    elements: &Vec<Located<ExpressionType>>,
    value: &Located<ExpressionType>,
) -> ExpressionType {
    let mut elems = vec![to_located(ExpressionType::NamedExpression {
        left: Box::from(to_located(ExpressionType::Identifier {
            name: SPLIT_NAME.to_string(),
        })),
        right: Box::from(to_located(clone_expression(&value.node))),
    })];

    for (i, target) in elements.iter().enumerate() {
        elems.push(to_located(inline_assign_statement(
            target,
            &to_located(ExpressionType::Subscript {
                a: Box::from(to_located(ExpressionType::Identifier {
                    name: SPLIT_NAME.to_string(),
                })),
                b: Box::from(to_located(ExpressionType::Number {
                    value: Number::Integer {
                        value: BigInt::from(i),
                    },
                })),
            }),
        )))
    }

    ExpressionType::List { elements: elems }
}

fn to_expression(statement: &StatementType, level: u32) -> ExpressionType {
    match statement {
        StatementType::Assign { targets, value } => {
            if targets.len() > 1 {
                ExpressionType::List {
                    elements: targets
                        .iter()
                        .map(|target| to_located(inline_assign_statement(&target, value)))
                        .collect::<Vec<Located<ExpressionType>>>(),
                }
            } else {
                inline_assign_statement(&targets[0], value)
            }
        }
        StatementType::AugAssign { target, op, value } => ExpressionType::NamedExpression {
            left: Box::from(to_located(clone_expression(&target.node))),
            right: Box::from(to_located(ExpressionType::Binop {
                a: Box::from(to_located(clone_expression(&target.node))),
                op: match op {
                    Operator::Add => Operator::Add,
                    Operator::Sub => Operator::Sub,
                    Operator::Mult => Operator::Mult,
                    Operator::MatMult => Operator::MatMult,
                    Operator::Div => Operator::Div,
                    Operator::Mod => Operator::Mod,
                    Operator::Pow => Operator::Pow,
                    Operator::LShift => Operator::LShift,
                    Operator::RShift => Operator::RShift,
                    Operator::BitOr => Operator::BitOr,
                    Operator::BitXor => Operator::BitXor,
                    Operator::BitAnd => Operator::BitAnd,
                    Operator::FloorDiv => Operator::FloorDiv,
                },
                b: Box::from(to_located(clone_expression(&value.node))),
            })),
        },
        StatementType::Expression { expression } => clone_expression(&expression.node),
        StatementType::FunctionDef {
            is_async: _,
            name,
            args,
            body,
            decorator_list,
            returns: _,
        } => {
            let mut lambda_statements = vec![to_located(reset_state())];

            for statement in prepare_body(&body, 0, 0) {
                lambda_statements.push(to_located(clone_expression(&statement.node)));
            }

            lambda_statements.push(to_located(ExpressionType::Subscript {
                a: Box::from(to_located(ExpressionType::Identifier {
                    name: STATE_NAME.to_string(),
                })),
                b: Box::from(to_located(ExpressionType::Number {
                    value: Number::Integer {
                        value: BigInt::from(0),
                    },
                })),
            }));

            let lambda_body = to_located(ExpressionType::Subscript {
                a: Box::from(to_located(ExpressionType::List {
                    elements: lambda_statements,
                })),
                b: Box::from(to_located(ExpressionType::Number {
                    value: Number::Integer {
                        value: BigInt::from(-1),
                    },
                })),
            });

            let lambda = ExpressionType::Lambda {
                args: Box::from(clone_parameters(args)),
                body: Box::from(lambda_body),
            };

            let decorated = decorator_list
                .iter()
                .rev()
                .fold(to_located(lambda), |acc, cur| {
                    to_located(ExpressionType::Call {
                        function: Box::from(to_located(clone_expression(&cur.node))),
                        args: vec![acc],
                        keywords: vec![],
                    })
                });

            let lambda_var = ExpressionType::NamedExpression {
                left: Box::from(to_located(ExpressionType::Identifier {
                    name: name.clone(),
                })),
                right: Box::from(decorated),
            };

            lambda_var
        }
        StatementType::For {
            is_async,
            target,
            iter,
            body,
            orelse,
        } => {
            // escaping fields are handled in the list comprehension
            let mut variants = HashSet::new();

            // Find variants in loop target
            clone_expression_adapter(&target.node, &mut |located| {
                if let ExpressionType::Identifier { name } = &located {
                    variants.insert(name.to_string());
                };

                located
            });

            // Add redeclare each in the local scope
            let mut local_body = Vec::from_iter(variants.clone())
                .iter()
                .map(|variant| {
                    to_located(ExpressionType::NamedExpression {
                        left: Box::from(to_located(ExpressionType::Identifier {
                            name: to_internal(variant.to_string()),
                        })),
                        right: Box::from(to_located(ExpressionType::Identifier {
                            name: variant.to_string(),
                        })),
                    })
                })
                .collect::<Vec<Located<ExpressionType>>>();

            let prepared_body = prepare_body(body, level, 0);

            for lin in prepared_body.iter() {
                let mut local_lin = to_located(clone_expression(&lin.node));

                local_lin.node = clone_expression_adapter(&local_lin.node, &mut |located| {
                    if let ExpressionType::Identifier { name } = &located {
                        if variants.contains(&name.to_string()) {
                            return ExpressionType::Identifier {
                                name: to_internal(name.to_string()),
                            };
                        }
                    };

                    located
                });

                local_body.push(local_lin);
            }
            let lambda_body = to_located(ExpressionType::List {
                elements: local_body,
            });

            let kind = ComprehensionKind::List {
                element: lambda_body,
            };

            let generators = vec![Comprehension {
                location: Location::new(0, 0),
                target: to_located(clone_expression(&target.node)),
                iter: to_located(clone_expression(&iter.node)),
                ifs: vec![],
                is_async: is_async.clone(),
            }];

            let comprehension = ExpressionType::Comprehension {
                kind: Box::from(kind),
                generators: generators,
            };

            if let Some(orelse) = orelse {
                let orelse_body = to_located(ExpressionType::List {
                    elements: orelse
                        .iter()
                        .map(|lin| to_located(to_expression(&lin.node, level + 1)))
                        .collect::<Vec<Located<ExpressionType>>>(),
                });

                ExpressionType::List {
                    elements: vec![to_located(comprehension), orelse_body],
                }
            } else {
                comprehension
            }
        }
        StatementType::While { test, body, orelse } => {
            // Find variants in test, add each as a param and replace with local name

            let mut local_test = to_located(clone_expression(&test.node));
            let mut variants = HashSet::new();
            let local_level = level + 1;

            local_test.node = clone_expression_adapter(&local_test.node, &mut |located| {
                if let ExpressionType::Identifier { name } = located {
                    variants.insert(name.to_string());

                    return ExpressionType::Identifier {
                        name: to_internal(name.to_string()),
                    };
                };

                located
            });

            // `__INL__COND := ...` => Compute the condition for each lambda call

            // `[...] if __INL__STATE[1] > 1 else None` => check for continuation after every point where a
            // statement that alter the flow of the program can act. If no return (etc.) was done before, it
            // will play the rest of the program in the [...], otherwise 
            
            // `... if __INL__COND else (__INL__STATE, ...)` => check in a while if the condition for continuing
            // was met, if so, it will play the current while body. Otherwise, it will return the state and all
            // locally copied variable that were potentially modified for update them in the upper scope

            let mut body_composite = prepare_body(body, local_level, 0);

            // Find each used variable used in the body to preemptively include them
            // from the external scope, in case if they are already present

            let mut variables = HashSet::new();

            for statement in &body_composite {
                clone_expression_adapter(&statement.node, &mut |located| {
                    if let ExpressionType::Identifier { name } = &located {
                        if !variants.contains(&name.to_string())
                            && name.to_string() != CORE_NAME.to_string()
                            && name.to_string() != STATE_NAME.to_string()
                        {
                            variables.insert(name.to_string());
                        }
                    };

                    located
                });
            }

            let args = [
                vec![CORE_NAME.to_string(), STATE_NAME.to_string()],
                Vec::from_iter(variants.clone()),
                Vec::from_iter(variables.clone()),
            ]
            .concat();

            let recursive_call = to_located(ExpressionType::Call {
                function: Box::from(to_located(ExpressionType::Identifier {
                    name: CORE_NAME.to_string(),
                })),
                args: args
                    .iter()
                    .map(|arg| to_located(ExpressionType::Identifier { name: arg.clone() }))
                    .collect::<Vec<Located<ExpressionType>>>(),
                keywords: vec![],
            });

            body_composite.push(recursive_call);

            let lambda_body = Box::from(to_located(ExpressionType::Subscript {
                a: Box::from(to_located(clone_expression_adapter(
                    &ExpressionType::List {
                        elements: body_composite,
                    },
                    &mut |located| {
                        if let ExpressionType::Identifier { name } = &located {
                            if variants.contains(&name.to_string())
                                || variables.contains(&name.to_string())
                            {
                                return ExpressionType::Identifier {
                                    name: to_internal(name.to_string()),
                                };
                            }
                        };

                        located
                    },
                ))),
                b: Box::from(to_located(ExpressionType::Number {
                    value: Number::Integer {
                        value: BigInt::from(-1),
                    },
                })),
            }));

            let mut core_body = vec![];

            let exports = vec![
                Vec::from_iter(variants.clone()),
                Vec::from_iter(variables.clone()),
            ]
            .concat();

            for variant in &exports {
                core_body.push(to_located(ExpressionType::NamedExpression {
                    left: Box::from(to_located(ExpressionType::Identifier {
                        name: to_internal(variant.to_string()),
                    })),
                    right: Box::from(to_located(ExpressionType::Identifier {
                        name: variant.to_string(),
                    })),
                }));
            }


            // Before executing the body of the current iteration, we need to check that
            // no changes have been made to the program flow in the last iteration body.
            let flow_local_test = to_located(ExpressionType::BoolOp { 
                op: BooleanOperator::And, 
                values: vec![
                    local_test,
                    to_located(check_state_target(local_level))
                ]
            });

            core_body.push(to_located(ExpressionType::NamedExpression {
                left: Box::from(to_located(ExpressionType::Identifier {
                    name: COND_NAME.to_string(),
                })),
                right: Box::from(flow_local_test),
            }));

            let mut return_state = vec![to_located(ExpressionType::Identifier {
                name: STATE_NAME.to_string(),
            })];

            if exports.len() > 0 {
                for variant in &exports {
                    return_state.push(to_located(ExpressionType::Identifier {
                        name: variant.to_string(),
                    }))
                }
            } else {
                return_state.push(to_located(ExpressionType::None));
            };

            core_body.push(to_located(ExpressionType::IfExpression {
                test: Box::from(to_located(ExpressionType::Identifier {
                    name: COND_NAME.to_string(),
                })),
                body: Box::from(lambda_body),
                orelse: Box::from(to_located(ExpressionType::Tuple {
                    elements: return_state,
                })),
            }));

            let core_lambda = ExpressionType::Lambda {
                args: Box::from(Parameters {
                    posonlyargs_count: 0,
                    args: args
                        .iter()
                        .map(|arg| Parameter {
                            location: Location::new(0, 0),
                            arg: arg.to_string(),
                            annotation: None,
                        })
                        .collect::<Vec<Parameter>>(),
                    kwonlyargs: vec![],
                    vararg: Varargs::None,
                    kwarg: Varargs::None,
                    defaults: vec![],
                    kw_defaults: vec![],
                }),
                body: Box::from(to_located(ExpressionType::Subscript {
                    a: Box::from(to_located(ExpressionType::List {
                        elements: core_body,
                    })),
                    b: Box::from(to_located(ExpressionType::Number {
                        value: Number::Integer {
                            value: BigInt::from(-1),
                        },
                    })),
                })),
            };

            let launch_lambda = ExpressionType::Lambda {
                args: Box::from(Parameters {
                    posonlyargs_count: 0,
                    args: args
                        .iter()
                        .map(|arg| Parameter {
                            location: Location::new(0, 0),
                            arg: arg.to_string(),
                            annotation: None,
                        })
                        .collect::<Vec<Parameter>>(),
                    kwonlyargs: vec![],
                    vararg: Varargs::None,
                    kwarg: Varargs::None,
                    defaults: vec![],
                    kw_defaults: vec![],
                }),
                body: Box::from(to_located(ExpressionType::Call {
                    function: Box::from(to_located(ExpressionType::Identifier {
                        name: CORE_NAME.to_string(),
                    })),
                    args: args
                        .iter()
                        .map(|arg| to_located(ExpressionType::Identifier { name: arg.clone() }))
                        .collect::<Vec<Located<ExpressionType>>>(),
                    keywords: vec![],
                })),
            };

            let mut call_arguments = vec![
                to_located(core_lambda),
                to_located(ExpressionType::Identifier {
                    name: STATE_NAME.to_string(),
                }),
            ];

            for variant in Vec::from_iter(variants.clone()) {
                call_arguments.push(to_located(ExpressionType::Identifier {
                    name: variant.to_string(),
                }));
            }

            for variable in Vec::from_iter(variables.clone()) {
                call_arguments.push(to_located(optional_variable(variable)));
            }

            let call_lambda = ExpressionType::NamedExpression {
                left: Box::from(to_located(ExpressionType::Identifier {
                    name: TEMP_NAME.to_string(),
                })),
                right: Box::from(to_located(ExpressionType::Call {
                    function: Box::from(to_located(launch_lambda)),
                    args: call_arguments,
                    keywords: vec![],
                })),
            };

            let mut call_operations = vec![];

            // Me must explicitely update the state with the current scope target
            call_operations.push(to_located(update_state(ExpressionType::None, local_level)));

            // We then call the recursive lambda
            call_operations.push(to_located(call_lambda));

            // Finally, we extract the state from the temp value of the lambda
            call_operations.push(to_located(ExpressionType::NamedExpression {
                left: Box::from(to_located(ExpressionType::Identifier {
                    name: STATE_NAME.to_string(),
                })),
                right: Box::from(to_located(ExpressionType::Subscript {
                    a: Box::from(to_located(ExpressionType::Identifier {
                        name: TEMP_NAME.to_string(),
                    })),
                    b: Box::from(to_located(ExpressionType::Number {
                        value: Number::Integer {
                            value: BigInt::from(0),
                        },
                    })),
                })),
            }));

            for (i, variant) in exports.iter().enumerate() {
                call_operations.push(to_located(ExpressionType::NamedExpression {
                    left: Box::from(to_located(ExpressionType::Identifier {
                        name: variant.to_string(),
                    })),
                    right: Box::from(to_located(ExpressionType::Subscript {
                        a: Box::from(to_located(ExpressionType::Identifier {
                            name: TEMP_NAME.to_string(),
                        })),
                        b: Box::from(to_located(ExpressionType::Number {
                            value: Number::Integer {
                                value: BigInt::from(i + 1),
                            },
                        })),
                    })),
                }));
            }

            if let Some(orelse) = orelse {
                call_operations.push(to_located(ExpressionType::List {
                    elements: orelse
                        .iter()
                        .map(|lin| to_located(to_expression(&lin.node, local_level)))
                        .collect::<Vec<Located<ExpressionType>>>(),
                }));
            };

            ExpressionType::List {
                elements: call_operations,
            }
        }
        StatementType::ClassDef {
            name,
            body,
            bases,
            keywords: _,
            decorator_list,
        } => {
            // A := type('A', (object,), {'m': "bar", 'foo': property(lambda self: self.m)})
            let mut local_bases = vec![to_located(ExpressionType::Identifier {
                name: "object".to_string(),
            })];

            for base in bases {
                local_bases.push(to_located(clone_expression(&base.node)))
            }

            let class_declaration = to_located(ExpressionType::Call {
                function: Box::from(to_located(ExpressionType::Identifier {
                    name: "type".to_string(),
                })),
                args: vec![
                    to_located(ExpressionType::String {
                        value: StringGroup::Constant {
                            value: name.to_string(),
                        },
                    }),
                    to_located(ExpressionType::Tuple {
                        elements: local_bases,
                    }),
                    to_located(ExpressionType::Dict {
                        elements: body
                            .iter()
                            .map(|lin| match to_expression(&lin.node, level + 0) {
                                ExpressionType::NamedExpression { left, right } => (
                                    Some(to_located(ExpressionType::String {
                                        value: StringGroup::Constant {
                                            value: serialize_expression(&left.node),
                                        },
                                    })),
                                    to_located(clone_expression(&right.node)),
                                ),
                                _ => panic!(
                                    "Object declaration can't contain non assignments statements"
                                ),
                            })
                            .collect::<Vec<_>>(),
                    }),
                ],
                keywords: vec![],
            });

            let decorated = decorator_list
                .iter()
                .rev()
                .fold(class_declaration, |acc, cur| {
                    to_located(ExpressionType::Call {
                        function: Box::from(to_located(clone_expression(&cur.node))),
                        args: vec![acc],
                        keywords: vec![],
                    })
                });

            ExpressionType::NamedExpression {
                left: Box::from(to_located(ExpressionType::Identifier {
                    name: name.to_string(),
                })),
                right: Box::from(decorated),
            }
        }
        StatementType::If { test, body, orelse } => ExpressionType::IfExpression {
            test: Box::from(to_located(clone_expression(&test.node))),
            body: Box::from(to_located(ExpressionType::List {
                elements: body
                    .iter()
                    .map(|lin| to_located(to_expression(&lin.node, level + 1)))
                    .collect::<Vec<Located<ExpressionType>>>(),
            })),
            orelse: Box::from(to_located(match orelse {
                Some(orelse) => ExpressionType::List {
                    elements: orelse
                        .iter()
                        .map(|lin| to_located(to_expression(&lin.node, level + 1)))
                        .collect::<Vec<Located<ExpressionType>>>(),
                },
                None => ExpressionType::None,
            })),
        },
        StatementType::Pass => update_state(ExpressionType::None, level),
        StatementType::Continue => update_state(ExpressionType::None, level),
        StatementType::Break => update_state(ExpressionType::None, level - 1),
        StatementType::Return { value } => match value {
            Some(value) => update_state(clone_expression(&value.node), 0),
            None => update_state(ExpressionType::None, 0),
        },
        StatementType::Import { names } => ExpressionType::List {
            elements: names
                .iter()
                .map(|name| {
                    to_located(ExpressionType::NamedExpression {
                        left: Box::from(to_located(ExpressionType::Identifier {
                            name: name.symbol.to_string(),
                        })),
                        right: Box::from(to_located(ExpressionType::Identifier {
                            name: format!("{}{}", IMPORT_KEY, name.symbol.to_string()),
                        })),
                    })
                })
                .collect::<Vec<Located<ExpressionType>>>(),
        },
        StatementType::ImportFrom {
            level: _,
            module: _,
            names,
        } => ExpressionType::List {
            elements: names
                .iter()
                .map(|name| {
                    to_located(ExpressionType::NamedExpression {
                        left: Box::from(to_located(ExpressionType::Identifier {
                            name: name.symbol.to_string(),
                        })),
                        right: Box::from(to_located(ExpressionType::Identifier {
                            name: format!("{}{}", IMPORT_KEY, name.symbol.to_string()),
                        })),
                    })
                })
                .collect::<Vec<Located<ExpressionType>>>(),
        },
        StatementType::Raise { exception, cause } => ExpressionType::Call {
            function: Box::from(to_located(ExpressionType::Attribute {
                value: Box::from(to_located(ExpressionType::Comprehension {
                    kind: Box::from(ComprehensionKind::GeneratorExpression {
                        element: to_located(ExpressionType::Identifier {
                            name: EMPTY_NAME.to_string(),
                        }),
                    }),
                    generators: vec![Comprehension {
                        location: Location::new(0, 0),
                        target: to_located(ExpressionType::Identifier {
                            name: EMPTY_NAME.to_string(),
                        }),
                        iter: to_located(ExpressionType::Tuple { elements: vec![] }),
                        ifs: vec![],
                        is_async: false,
                    }],
                })),
                name: THROW_NAME.to_string(),
            })),
            args: vec![to_located(ExpressionType::Call {
                function: Box::from(to_located(match &exception {
                    Some(exception) => clone_expression(&exception.node),
                    None => ExpressionType::Identifier {
                        name: EXCEPTION_DEF_TYPE.to_string(),
                    },
                })),
                args: vec![to_located(match &cause {
                    Some(cause) => clone_expression(&cause.node),
                    None => ExpressionType::String {
                        value: StringGroup::Constant {
                            value: EXCEPTION_DEF_MSG.to_string(),
                        },
                    },
                })],
                keywords: vec![],
            })],
            keywords: vec![],
        },
        StatementType::Try {
            body: _,
            handlers: _,
            orelse: _,
            finalbody: _,
        } => {
            // sys := __INL__IMPORT_sys
            // tmp = sys.excepthook
            // rest = (lambda: [])
            // sys.excepthook = (lambda type, val, tbk: [print(val), rest()])
            // rest()
            let mut elements = vec![];

            elements.push(to_located(ExpressionType::NamedExpression {
                left: Box::from(to_located(ExpressionType::Identifier {
                    name: SYSTEM_NAME.to_string(),
                })),
                right: Box::from(to_located(ExpressionType::Identifier {
                    name: format!("{}{}", IMPORT_KEY.to_string(), SYSTEM_NAME.to_string()),
                })),
            }));

            elements.push(to_located(ExpressionType::NamedExpression {
                left: Box::from(to_located(ExpressionType::Identifier {
                    name: TEMP_EXCEPTHOOK.to_string(),
                })),
                right: Box::from(to_located(ExpressionType::Attribute {
                    value: Box::from(to_located(ExpressionType::Identifier {
                        name: SYSTEM_NAME.to_string(),
                    })),
                    name: EXCEPTHOOK_NAME.to_string(),
                })),
            }));

            // TODO

            elements.push(to_located(ExpressionType::Call {
                function: Box::from(to_located(ExpressionType::Identifier {
                    name: SETATTR_NAME.to_string(),
                })),
                args: vec![
                    to_located(ExpressionType::Identifier {
                        name: SYSTEM_NAME.to_string(),
                    }),
                    to_located(ExpressionType::String {
                        value: StringGroup::Constant {
                            value: EXCEPTHOOK_NAME.to_string(),
                        },
                    }),
                    to_located(ExpressionType::Identifier {
                        name: TEMP_EXCEPTHOOK.to_string(),
                    }),
                ],
                keywords: vec![],
            }));

            ExpressionType::List { elements }
        }
        StatementType::Delete { targets } => ExpressionType::List {
            elements: targets
                .iter()
                .map(|target| match &target.node {
                    ExpressionType::Subscript { a, b } => {
                        // a.__delitem__(b)
                        to_located(ExpressionType::Call {
                            function: Box::from(to_located(ExpressionType::Attribute {
                                value: Box::from(to_located(clone_expression(&a.node))),
                                name: DELITEM_NAME.to_string(),
                            })),
                            args: vec![match &b.node {
                                ExpressionType::Slice { elements } => {
                                    to_located(ExpressionType::Call {
                                        function: Box::from(to_located(
                                            ExpressionType::Identifier {
                                                name: SLICE_NAME.to_string(),
                                            },
                                        )),
                                        args: elements.iter().map(|e| to_located(clone_expression(&e.node))).collect::<Vec<Located<ExpressionType>>>(),
                                        keywords: vec![],
                                    })
                                }
                                _ => to_located(clone_expression(&b.node)),
                            }],
                            keywords: vec![],
                        })
                    }
                    ExpressionType::Attribute { value, name } => {
                        // delattr(value, "name")
                        to_located(ExpressionType::Call {
                            function: Box::from(to_located(ExpressionType::Identifier {
                                name: DELATTR_NAME.to_string(),
                            })),
                            args: vec![
                                to_located(clone_expression(&value.node)),
                                to_located(ExpressionType::String {
                                    value: StringGroup::Constant {
                                        value: name.to_string(),
                                    },
                                }),
                            ],
                            keywords: vec![],
                        })
                    }
                    _ => {
                        let program = parse(&format!("[__INL__frame := __INL__IMPORT_inspect.currentframe(), [__INL__frame.f_locals.pop(__INL__name) for __INL__name in [__INL__name for __INL__name, __INL__value in __INL__frame.f_locals.items() if __INL__value is {}]], __INL__IMPORT_ctypes.pythonapi.PyFrame_LocalsToFast(__INL__IMPORT_ctypes.py_object(__INL__frame), __INL__IMPORT_ctypes.c_int(1))]", serialize_expression(&target.node)));
                        to_located(to_expression(&program.statements[0].node, level + 1))
                    }
                })
                .collect::<Vec<Located<ExpressionType>>>(),
        },
        _ => todo!(),
    }
}

fn to_located<T>(node: T) -> Located<T> {
    Located {
        location: Location::new(0, 0),
        node: node,
    }
}

fn clone_expression(expression: &ExpressionType) -> ExpressionType {
    clone_expression_adapter(expression, &mut |located| located)
}

fn clone_parameters(args: &Parameters) -> Parameters {
    clone_parameters_adapter(args, &mut |located| located)
}

fn clone_expression_adapter(
    expression: &ExpressionType,
    f: &mut impl FnMut(ExpressionType) -> ExpressionType,
) -> ExpressionType {
    let local_expression = match expression {
        ExpressionType::Attribute { value, name } => ExpressionType::Attribute {
            value: Box::from(to_located(clone_expression_adapter(&value.node, f))),
            name: name.clone(),
        },
        ExpressionType::BoolOp { op, values } => ExpressionType::BoolOp {
            op: match op {
                BooleanOperator::And => BooleanOperator::And,
                BooleanOperator::Or => BooleanOperator::Or,
            },
            values: values
                .iter()
                .map(|value| to_located(clone_expression_adapter(&value.node, f)))
                .collect::<Vec<Located<ExpressionType>>>(),
        },
        ExpressionType::Binop { a, op, b } => ExpressionType::Binop {
            a: Box::from(to_located(clone_expression_adapter(&&a.node, f))),
            op: match op {
                Operator::Add => Operator::Add,
                Operator::Sub => Operator::Sub,
                Operator::Mult => Operator::Mult,
                Operator::MatMult => Operator::MatMult,
                Operator::Div => Operator::Div,
                Operator::Mod => Operator::Mod,
                Operator::Pow => Operator::Pow,
                Operator::LShift => Operator::LShift,
                Operator::RShift => Operator::RShift,
                Operator::BitOr => Operator::BitOr,
                Operator::BitXor => Operator::BitXor,
                Operator::BitAnd => Operator::BitAnd,
                Operator::FloorDiv => Operator::FloorDiv,
            },
            b: Box::from(to_located(clone_expression_adapter(&&b.node, f))),
        },
        ExpressionType::Subscript { a, b } => ExpressionType::Subscript {
            a: Box::from(to_located(clone_expression_adapter(&a.node, f))),
            b: Box::from(to_located(clone_expression_adapter(&b.node, f))),
        },
        ExpressionType::Unop { op, a } => ExpressionType::Unop {
            op: match op {
                UnaryOperator::Pos => UnaryOperator::Pos,
                UnaryOperator::Neg => UnaryOperator::Neg,
                UnaryOperator::Not => UnaryOperator::Not,
                UnaryOperator::Inv => UnaryOperator::Inv,
            },
            a: Box::from(to_located(clone_expression_adapter(&a.node, f))),
        },
        ExpressionType::Await { value } => ExpressionType::Await {
            value: Box::from(to_located(clone_expression_adapter(&value.node, f))),
        },
        ExpressionType::Yield { value } => ExpressionType::Yield {
            value: match value {
                Some(value) => Some(Box::from(to_located(clone_expression_adapter(
                    &value.node,
                    f,
                )))),
                None => None,
            },
        },
        ExpressionType::YieldFrom { value } => ExpressionType::YieldFrom {
            value: Box::from(to_located(clone_expression_adapter(&value.node, f))),
        },
        ExpressionType::Compare { vals, ops } => ExpressionType::Compare {
            vals: vals
                .iter()
                .map(|val| to_located(clone_expression_adapter(&val.node, f)))
                .collect::<Vec<Located<ExpressionType>>>(),
            ops: ops
                .iter()
                .map(|op| match op {
                    Comparison::Equal => Comparison::Equal,
                    Comparison::NotEqual => Comparison::NotEqual,
                    Comparison::Less => Comparison::Less,
                    Comparison::LessOrEqual => Comparison::LessOrEqual,
                    Comparison::Greater => Comparison::Greater,
                    Comparison::GreaterOrEqual => Comparison::GreaterOrEqual,
                    Comparison::In => Comparison::In,
                    Comparison::NotIn => Comparison::NotIn,
                    Comparison::Is => Comparison::Is,
                    Comparison::IsNot => Comparison::IsNot,
                })
                .collect::<Vec<Comparison>>(),
        },
        ExpressionType::Call {
            function,
            args,
            keywords,
        } => ExpressionType::Call {
            function: Box::from(to_located(clone_expression_adapter(&function.node, f))),
            args: args
                .iter()
                .map(|arg| to_located(clone_expression_adapter(&arg.node, f)))
                .collect::<Vec<Located<ExpressionType>>>(),
            keywords: keywords
                .iter()
                .map(|keyword| Keyword {
                    name: keyword.name.clone(),
                    value: to_located(clone_expression_adapter(&&keyword.value.node, f)),
                })
                .collect::<Vec<Keyword>>(),
        },
        ExpressionType::Number { value } => ExpressionType::Number {
            value: match value {
                Number::Integer { value } => Number::Integer {
                    value: value.clone(),
                },
                Number::Float { value } => Number::Float {
                    value: value.clone(),
                },
                Number::Complex { real, imag } => Number::Complex {
                    real: real.clone(),
                    imag: imag.clone(),
                },
            },
        },
        ExpressionType::List { elements } => ExpressionType::List {
            elements: elements
                .iter()
                .map(|element| to_located(clone_expression_adapter(&element.node, f)))
                .collect::<Vec<Located<ExpressionType>>>(),
        },
        ExpressionType::Tuple { elements } => ExpressionType::Tuple {
            elements: elements
                .iter()
                .map(|element| to_located(clone_expression_adapter(&element.node, f)))
                .collect::<Vec<Located<ExpressionType>>>(),
        },
        ExpressionType::Dict { elements } => ExpressionType::Dict {
            elements: elements
                .iter()
                .map(|(key, value)| match key {
                    Some(key) => (
                        Some(to_located(clone_expression_adapter(&key.node, f))),
                        to_located(clone_expression_adapter(&value.node, f)),
                    ),
                    None => (None, to_located(clone_expression_adapter(&value.node, f))),
                })
                .collect::<Vec<(Option<Located<ExpressionType>>, Located<ExpressionType>)>>(),
        },
        ExpressionType::Set { elements } => ExpressionType::Set {
            elements: elements
                .iter()
                .map(|element| to_located(clone_expression_adapter(&element.node, f)))
                .collect::<Vec<Located<ExpressionType>>>(),
        },
        ExpressionType::Comprehension { kind, generators } => ExpressionType::Comprehension {
            kind: Box::from(match kind.as_ref() {
                ComprehensionKind::GeneratorExpression { element } => {
                    ComprehensionKind::GeneratorExpression {
                        element: to_located(clone_expression_adapter(&element.node, f)),
                    }
                }
                ComprehensionKind::List { element } => ComprehensionKind::List {
                    element: to_located(clone_expression_adapter(&element.node, f)),
                },
                ComprehensionKind::Set { element } => ComprehensionKind::Set {
                    element: to_located(clone_expression_adapter(&element.node, f)),
                },
                ComprehensionKind::Dict { key, value } => ComprehensionKind::Dict {
                    key: to_located(clone_expression_adapter(&key.node, f)),
                    value: to_located(clone_expression_adapter(&value.node, f)),
                },
            }),
            generators: generators
                .iter()
                .map(|generator| Comprehension {
                    location: Location::new(0, 0),
                    target: to_located(clone_expression_adapter(&&generator.target.node, f)),
                    iter: to_located(clone_expression_adapter(&&generator.iter.node, f)),
                    ifs: generator
                        .ifs
                        .iter()
                        .map(|i| to_located(clone_expression_adapter(&i.node, f)))
                        .collect::<Vec<Located<ExpressionType>>>(),
                    is_async: generator.is_async,
                })
                .collect::<Vec<Comprehension>>(),
        },
        ExpressionType::Starred { value } => ExpressionType::Starred {
            value: Box::from(to_located(clone_expression_adapter(&value.node, f))),
        },
        ExpressionType::Slice { elements } => ExpressionType::Slice {
            elements: elements
                .iter()
                .map(|element| to_located(clone_expression_adapter(&element.node, f)))
                .collect::<Vec<Located<ExpressionType>>>(),
        },
        ExpressionType::String { value } => {
            fn handler(
                value: &StringGroup,
                f: &mut impl FnMut(ExpressionType) -> ExpressionType,
            ) -> StringGroup {
                match value {
                    StringGroup::Constant { value } => StringGroup::Constant {
                        value: value.clone(),
                    },
                    StringGroup::FormattedValue {
                        value,
                        conversion,
                        spec,
                    } => StringGroup::FormattedValue {
                        value: Box::from(to_located(clone_expression_adapter(&value.node, f))),
                        conversion: match conversion {
                            Some(conversion) => Some(match conversion {
                                ConversionFlag::Str => ConversionFlag::Str,
                                ConversionFlag::Ascii => ConversionFlag::Ascii,
                                ConversionFlag::Repr => ConversionFlag::Repr,
                            }),
                            None => None,
                        },
                        spec: match spec {
                            Some(spec) => Some(Box::from(handler(spec, f))),
                            None => None,
                        },
                    },
                    StringGroup::Joined { values } => StringGroup::Joined {
                        values: values
                            .iter()
                            .map(|value| handler(value, f))
                            .collect::<Vec<StringGroup>>(),
                    },
                }
            }

            ExpressionType::String {
                value: handler(value, f),
            }
        }
        ExpressionType::Bytes { value } => ExpressionType::Bytes {
            value: value.clone(),
        },
        ExpressionType::Identifier { name } => ExpressionType::Identifier { name: name.clone() },
        ExpressionType::Lambda { args, body } => ExpressionType::Lambda {
            args: Box::from(clone_parameters(args)),
            body: Box::from(to_located(clone_expression_adapter(&body.node, f))),
        },
        ExpressionType::IfExpression { test, body, orelse } => ExpressionType::IfExpression {
            test: Box::from(to_located(clone_expression_adapter(&test.node, f))),
            body: Box::from(to_located(clone_expression_adapter(&body.node, f))),
            orelse: Box::from(to_located(clone_expression_adapter(&orelse.node, f))),
        },
        ExpressionType::NamedExpression { left, right } => ExpressionType::NamedExpression {
            left: Box::from(to_located(clone_expression_adapter(&left.node, f))),
            right: Box::from(to_located(clone_expression_adapter(&right.node, f))),
        },
        ExpressionType::True => ExpressionType::True,
        ExpressionType::False => ExpressionType::False,
        ExpressionType::None => ExpressionType::None,
        ExpressionType::Ellipsis => ExpressionType::Ellipsis,
    };

    f(local_expression)
}

fn clone_parameters_adapter(
    args: &Parameters,
    f: &mut impl FnMut(ExpressionType) -> ExpressionType,
) -> Parameters {
    Parameters {
        posonlyargs_count: args.posonlyargs_count,
        args: args
            .args
            .iter()
            .map(|arg| Parameter {
                location: Location::new(0, 0),
                arg: arg.arg.clone(),
                annotation: match &arg.annotation {
                    Some(i) => Some(Box::from(to_located(clone_expression_adapter(&i.node, f)))),
                    None => None,
                },
            })
            .collect::<Vec<Parameter>>(),
        kwonlyargs: args
            .kwonlyargs
            .iter()
            .map(|arg| Parameter {
                location: Location::new(0, 0),
                arg: arg.arg.clone(),
                annotation: match &arg.annotation {
                    Some(i) => Some(Box::from(to_located(clone_expression_adapter(&&i.node, f)))),
                    None => None,
                },
            })
            .collect::<Vec<Parameter>>(),
        vararg: match &args.vararg {
            Varargs::None => Varargs::None,
            Varargs::Unnamed => Varargs::Unnamed,
            Varargs::Named(varang) => Varargs::Named(Parameter {
                location: Location::new(0, 0),
                arg: varang.arg.clone(),
                annotation: match &varang.annotation {
                    Some(i) => Some(Box::from(to_located(clone_expression_adapter(&&i.node, f)))),
                    None => None,
                },
            }),
        },
        kwarg: match &args.kwarg {
            Varargs::None => Varargs::None,
            Varargs::Unnamed => Varargs::Unnamed,
            Varargs::Named(varang) => Varargs::Named(Parameter {
                location: Location::new(0, 0),
                arg: varang.arg.clone(),
                annotation: match &varang.annotation {
                    Some(i) => Some(Box::from(to_located(clone_expression_adapter(&&i.node, f)))),
                    None => None,
                },
            }),
        },
        defaults: args
            .defaults
            .iter()
            .map(|i| to_located(clone_expression_adapter(&i.node, f)))
            .collect::<Vec<Located<ExpressionType>>>(),
        kw_defaults: args
            .kw_defaults
            .iter()
            .map(|i| match i {
                Some(i) => Some(to_located(clone_expression_adapter(&i.node, f))),
                None => None,
            })
            .collect::<Vec<Option<Located<ExpressionType>>>>(),
    }
}

#[cfg(test)]
mod tests {
    use crate::python::{parse, serialize_inlined};
    use crate::transform::oneline::oneline;

    #[test]
    fn empty_program() {
        let source = "";
        let expect = "[__INL__STATE := (None, 1)]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn variable_declaration() {
        let source = "a = 1";
        let expect = "[__INL__STATE := (None, 1), a := 1]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn variable_declaration_multi() {
        let source = "a = 1\nb = a";
        let expect = "[__INL__STATE := (None, 1), a := 1, b := a]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn variable_bin_operation() {
        let source = "a = 1\nb = a + 2";
        let expect = "[__INL__STATE := (None, 1), a := 1, b := a + 2]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn function_call() {
        let source = "a = 1\nb = a + 2\nc = a + b\nprint(c)";
        let expect = "[__INL__STATE := (None, 1), a := 1, b := a + 2, c := a + b, print(c)]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn type_list() {
        let source = "a = [1, 2, 3]";
        let expect = "[__INL__STATE := (None, 1), a := [1, 2, 3]]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn type_tuple() {
        let source = "a = (1, 2, 3)";
        let expect = "[__INL__STATE := (None, 1), a := (1, 2, 3)]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn type_set() {
        let source = "a = { 1, 2, 3 }";
        let expect = "[__INL__STATE := (None, 1), a := { 1, 2, 3 }]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn type_dict() {
        let source = "a = { 'a': 1, 'b': 2, 'c': 3 }";
        let expect = "[__INL__STATE := (None, 1), a := { 'a': 1, 'b': 2, 'c': 3 }]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn type_string() {
        let source = "a = 'abc'";
        let expect = "[__INL__STATE := (None, 1), a := 'abc']";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn if_statement() {
        let source = "if a:\n\tprint(1)";
        let expect = "[__INL__STATE := (None, 1), [print(1)] if a else None, [] if __INL__STATE[1] > 0 else None]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn if_else_statement() {
        let source = "if a:\n\tprint(1)\nelse:\n\tprint(2)";
        let expect = "[__INL__STATE := (None, 1), [print(1)] if a else [print(2)], [] if __INL__STATE[1] > 0 else None]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn list_comprehension() {
        let source = "a = [i for i in range(10)]";
        let expect = "[__INL__STATE := (None, 1), a := [i for i in range(10)]]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn lambda_function() {
        let source = "a = (lambda x: x + 1)(1)";
        let expect = "[__INL__STATE := (None, 1), a := (lambda x: x + 1)(1)]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn function_declaration() {
        let source = "a = 1\ndef sprint(v):\n\tprint(v)\nsprint(a)";
        let expect = "[__INL__STATE := (None, 1), a := 1, sprint := (lambda v: [__INL__STATE := (None, 1), print(v), __INL__STATE[0]][-1]), sprint(a)]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn function_declaration_decorated() {
        let source = "a = 1\n@property\ndef sprint(v):\n\tprint(v)\nsprint(a)";
        let expect = "[__INL__STATE := (None, 1), a := 1, sprint := property((lambda v: [__INL__STATE := (None, 1), print(v), __INL__STATE[0]][-1])), sprint(a)]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn function_declaration_recursive() {
        let source =
            "def fib(i):\n\tif i <= 1:\n\t\treturn i\n\telse:\n\t\treturn fib(i-1) + f(i-2)";
        let expect = "[__INL__STATE := (None, 1), fib := (lambda i: [__INL__STATE := (None, 1), [__INL__STATE := (i, 0)] if i <= 1 else [__INL__STATE := (fib(i - 1) + f(i - 2), 0)], [] if __INL__STATE[1] > 0 else None, __INL__STATE[0]][-1])]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn function_declaration_parameter() {
        let source = "a = 1\ndef sprint(v):\n\tprint(v)\nsprint(a)";
        let expect = "[__INL__STATE := (None, 1), a := 1, sprint := (lambda v: [__INL__STATE := (None, 1), print(v), __INL__STATE[0]][-1]), sprint(a)]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn for_loop() {
        let source = "for i in range(10):\n\tprint(i)";
        let expect = "[__INL__STATE := (None, 1), [[__INL__i := i, print(__INL__i)] for i in range(10)], [] if __INL__STATE[1] > 0 else None]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn for_loop_else() {
        let source = "for i in range(10):\n\tprint(i)\nelse:\n\tprint('none')";
        let expect = "[__INL__STATE := (None, 1), [[[__INL__i := i, print(__INL__i)] for i in range(10)], [print('none')]], [] if __INL__STATE[1] > 0 else None]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn while_loop() {
        let source = "while True:\n\tprint('test')";
        let expect = "[__INL__STATE := (None, 1), [__INL__STATE := (None, 2), __INL__TEMP := (lambda __INL__CORE, __INL__STATE, print: __INL__CORE(__INL__CORE, __INL__STATE, print))((lambda __INL__CORE, __INL__STATE, print: [__INL__print := print, __INL__COND := True and __INL__STATE[1] > 1, [__INL__print('test'), __INL__CORE(__INL__CORE, __INL__STATE, __INL__print)][-1] if __INL__COND else (__INL__STATE, print)][-1]), __INL__STATE, print if 'print' in vars() else __builtins__.print if hasattr(__builtins__, 'print') else None), __INL__STATE := __INL__TEMP[0], print := __INL__TEMP[1]], [] if __INL__STATE[1] > 0 else None]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn while_loop_args() {
        let source = "a = 1\nwhile a < 5:\n\ta += 1\n\tprint(a)\nprint(a)";
        let expect = "[__INL__STATE := (None, 1), a := 1, [__INL__STATE := (None, 2), __INL__TEMP := (lambda __INL__CORE, __INL__STATE, a, print: __INL__CORE(__INL__CORE, __INL__STATE, a, print))((lambda __INL__CORE, __INL__STATE, a, print: [__INL__a := a, __INL__print := print, __INL__COND := __INL__a < 5 and __INL__STATE[1] > 1, [__INL__a := __INL__a + 1, __INL__print(__INL__a), __INL__CORE(__INL__CORE, __INL__STATE, __INL__a, __INL__print)][-1] if __INL__COND else (__INL__STATE, a, print)][-1]), __INL__STATE, a, print if 'print' in vars() else __builtins__.print if hasattr(__builtins__, 'print') else None), __INL__STATE := __INL__TEMP[0], a := __INL__TEMP[1], print := __INL__TEMP[2]], [print(a)] if __INL__STATE[1] > 0 else None]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn while_loop_args_else() {
        let source = "a = 1\nwhile a < 5:\n\ta += 1\n\tprint(a)\nelse:\n\tprint(a)";
        let expect = "[__INL__STATE := (None, 1), a := 1, [__INL__STATE := (None, 2), __INL__TEMP := (lambda __INL__CORE, __INL__STATE, a, print: __INL__CORE(__INL__CORE, __INL__STATE, a, print))((lambda __INL__CORE, __INL__STATE, a, print: [__INL__a := a, __INL__print := print, __INL__COND := __INL__a < 5 and __INL__STATE[1] > 1, [__INL__a := __INL__a + 1, __INL__print(__INL__a), __INL__CORE(__INL__CORE, __INL__STATE, __INL__a, __INL__print)][-1] if __INL__COND else (__INL__STATE, a, print)][-1]), __INL__STATE, a, print if 'print' in vars() else __builtins__.print if hasattr(__builtins__, 'print') else None), __INL__STATE := __INL__TEMP[0], a := __INL__TEMP[1], print := __INL__TEMP[2], [print(a)]], [] if __INL__STATE[1] > 0 else None]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn while_loop_flow_alteration() {
        let source = "while True:\n\tif True:\n\t\tbreak";
        let expect = "[__INL__STATE := (None, 1), [__INL__STATE := (None, 2), __INL__TEMP := (lambda __INL__CORE, __INL__STATE: __INL__CORE(__INL__CORE, __INL__STATE))((lambda __INL__CORE, __INL__STATE: [__INL__COND := True and __INL__STATE[1] > 1, [[__INL__STATE := (None, 2)] if True else None, [] if __INL__STATE[1] > 1 else None, __INL__CORE(__INL__CORE, __INL__STATE)][-1] if __INL__COND else (__INL__STATE, None)][-1]), __INL__STATE), __INL__STATE := __INL__TEMP[0]], [] if __INL__STATE[1] > 0 else None]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn class_declaration() {
        let source = "class A():\n\tm = 'bar'\n\tdef foo(self):\n\t\treturn self.m";
        let expect = "[__INL__STATE := (None, 1), A := type('A', (object,), { 'm': 'bar', 'foo': (lambda self: [__INL__STATE := (None, 1), __INL__STATE := (self.m, 0), __INL__STATE[0]][-1]) })]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn remove_after_return() {
        let source = "a = 1\nreturn\nb = 1";
        let expect = "[__INL__STATE := (None, 1), a := 1, __INL__STATE := (None, 0)]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn split_after_return() {
        let source = "if a == 1:\n\treturn\nb = 1";
        let expect = "[__INL__STATE := (None, 1), [__INL__STATE := (None, 0)] if a == 1 else None, [b := 1] if __INL__STATE[1] > 0 else None]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn function_return() {
        let source = "def a():\n\tif True:\n\t\treturn 1\n\treturn 2\nprint(a())";
        let expect = "[__INL__STATE := (None, 1), a := (lambda: [__INL__STATE := (None, 1), [__INL__STATE := (1, 0)] if True else None, [__INL__STATE := (2, 0)] if __INL__STATE[1] > 0 else None, __INL__STATE[0]][-1]), print(a())]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn optional_variable() {
        let source = "a = 1\nb = 2\nwhile a < 5:\n\ta += 1\n\tb -= 1\nprint(a)\nprint(b)";
        let expect = "[__INL__STATE := (None, 1), a := 1, b := 2, [__INL__TEMP := (lambda __INL__CORE, __INL__STATE, a, b: __INL__CORE(__INL__CORE, __INL__STATE, a, b))((lambda __INL__CORE, __INL__STATE, a, b: [__INL__a := a, __INL__b := b, __INL__COND := __INL__a < 5 and __INL__STATE[1] > 1, [__INL__a := __INL__a + 1, __INL__b := __INL__b - 1, __INL__CORE(__INL__CORE, __INL__STATE, __INL__a, __INL__b)][-1] if __INL__COND else (__INL__STATE, a, b)][-1]), __INL__STATE, a, b if 'b' in vars() else __builtins__.b if hasattr(__builtins__, 'b') else None), __INL__STATE := __INL__TEMP[0], a := __INL__TEMP[1], b := __INL__TEMP[2]], [print(a), print(b)] if __INL__STATE[1] > 0 else None]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn optional_function() {
        let source = "a = 1\nwhile a < 5:\n\ta += 1\n\tprint(a)";
        let expect = "[__INL__STATE := (None, 1), a := 1, [__INL__TEMP := (lambda __INL__CORE, __INL__STATE, a, print: __INL__CORE(__INL__CORE, __INL__STATE, a, print))((lambda __INL__CORE, __INL__STATE, a, print: [__INL__a := a, __INL__print := print, __INL__COND := __INL__a < 5 and __INL__STATE[1] > 1, [__INL__a := __INL__a + 1, __INL__print(__INL__a), __INL__CORE(__INL__CORE, __INL__STATE, __INL__a, __INL__print)][-1] if __INL__COND else (__INL__STATE, a, print)][-1]), __INL__STATE, a, print if 'print' in vars() else __builtins__.print if hasattr(__builtins__, 'print') else None), __INL__STATE := __INL__TEMP[0], a := __INL__TEMP[1], print := __INL__TEMP[2]], [] if __INL__STATE[1] > 0 else None]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn optional_function_returned() {
        let source = "a = 1\nwhile a < 5:\n\ta += 1\n\tprint(a)\nprint(a)";
        let expect = "[__INL__STATE := (None, 1), a := 1, [__INL__TEMP := (lambda __INL__CORE, __INL__STATE, a, print: __INL__CORE(__INL__CORE, __INL__STATE, a, print))((lambda __INL__CORE, __INL__STATE, a, print: [__INL__a := a, __INL__print := print, __INL__COND := __INL__a < 5 and __INL__STATE[1] > 1, [__INL__a := __INL__a + 1, __INL__print(__INL__a), __INL__CORE(__INL__CORE, __INL__STATE, __INL__a, __INL__print)][-1] if __INL__COND else (__INL__STATE, a, print)][-1]), __INL__STATE, a, print if 'print' in vars() else __builtins__.print if hasattr(__builtins__, 'print') else None), __INL__STATE := __INL__TEMP[0], a := __INL__TEMP[1], print := __INL__TEMP[2]], [print(a)] if __INL__STATE[1] > 0 else None]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn optional_function_scoped() {
        let source = "def main():\n\ta = 1\n\twhile a < 10:\n\t\ta += 1\n\t\tprint(a)\nmain()";
        let expect = "[__INL__STATE := (None, 1), main := (lambda: [__INL__STATE := (None, 1), a := 1, [__INL__TEMP := (lambda __INL__CORE, __INL__STATE, a, print: __INL__CORE(__INL__CORE, __INL__STATE, a, print))((lambda __INL__CORE, __INL__STATE, a, print: [__INL__a := a, __INL__print := print, __INL__COND := __INL__a < 10 and __INL__STATE[1] > 1, [__INL__a := __INL__a + 1, __INL__print(__INL__a), __INL__CORE(__INL__CORE, __INL__STATE, __INL__a, __INL__print)][-1] if __INL__COND else (__INL__STATE, a, print)][-1]), __INL__STATE, a, print if 'print' in vars() else __builtins__.print if hasattr(__builtins__, 'print') else None), __INL__STATE := __INL__TEMP[0], a := __INL__TEMP[1], print := __INL__TEMP[2]], [] if __INL__STATE[1] > 0 else None, __INL__STATE[0]][-1]), main()]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn import_statement() {
        let source = "import math\nprint(math.pi)";
        let expect =
            "import math as __INL__IMPORT_math;[__INL__STATE := (None, 1), [math := __INL__IMPORT_math], print(math.pi)]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn global_statement() {
        let source = "global a";
        let expect =
            "[__INL__STATE := (None, 1), a := globals()['a'], globals().update({ 'a': a })]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn global_statement_update() {
        let source = "a = 1\ndef test():\n\tglobal a\n\ta = 2";
        let expect =
            "[__INL__STATE := (None, 1), a := 1, test := (lambda: [__INL__STATE := (None, 1), a := globals()['a'], a := 2, globals().update({ 'a': a }), __INL__STATE[0]][-1])]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn assign_statement() {
        let source = "a = 1";
        let expect = "[__INL__STATE := (None, 1), a := 1]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn assign_statement_attribute() {
        let source = "a.attr = 1";
        let expect = "[__INL__STATE := (None, 1), setattr(a, 'attr', 1)]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn assign_statement_subscript() {
        let source = "a[1] = 1";
        let expect = "[__INL__STATE := (None, 1), a.__setitem__(1, 1)]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn assign_statement_multi() {
        let source = "a = b = 1";
        let expect = "[__INL__STATE := (None, 1), [a := 1, b := 1]]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn assign_statement_tuple() {
        let source = "(a, b) = (1, 2)";
        let expect = "[__INL__STATE := (None, 1), [__INL__SPLIT := (1, 2), a := __INL__SPLIT[0], b := __INL__SPLIT[1]]]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn assign_statement_list() {
        let source = "[a, b] = [1, 2]";
        let expect = "[__INL__STATE := (None, 1), [__INL__SPLIT := [1, 2], a := __INL__SPLIT[0], b := __INL__SPLIT[1]]]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn slice_array() {
        let source = "s[:]";
        let expect = "[__INL__STATE := (None, 1), s[:]]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn raise_exception() {
        let source = "raise";
        let expect = "[__INL__STATE := (None, 1), (_ for _ in ()).throw(RuntimeError('No active exception to reraise'))]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn raise_exception_message() {
        let source = "raise Exception('error')";
        let expect = "[__INL__STATE := (None, 1), (_ for _ in ()).throw(Exception('error')('No active exception to reraise'))]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn try_except() {
        let source = "try:\n\tprint('test')\nexcept:\n\tprint('error')";
        let expect = "import sys as __INL__IMPORT_sys;[__INL__STATE := (None, 1), [sys := __INL__IMPORT_sys, __INL__EXCEPTHOOK := sys.excepthook, setattr(sys, 'excepthook', __INL__EXCEPTHOOK)]]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn del_variable() {
        let source = "del a";
        let expect = "import inspect as __INL__IMPORT_inspect;import ctypes as __INL__IMPORT_ctypes;[__INL__STATE := (None, 1), [[__INL__frame := __INL__IMPORT_inspect.currentframe(), [__INL__frame.f_locals.pop(__INL__name) for __INL__name in [__INL__name for (__INL__name, __INL__value) in __INL__frame.f_locals.items() if __INL__value is a]], __INL__IMPORT_ctypes.pythonapi.PyFrame_LocalsToFast(__INL__IMPORT_ctypes.py_object(__INL__frame), __INL__IMPORT_ctypes.c_int(1))]]]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn del_item() {
        let source = "del l[0]";
        let expect = "[__INL__STATE := (None, 1), [l.__delitem__(0)]]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }

    #[test]
    fn del_slice() {
        let source = "del l[1:2]";
        let expect = "[__INL__STATE := (None, 1), [l.__delitem__(slice(1, 2, None))]]";
        assert_eq!(serialize_inlined(oneline(parse(source))), expect)
    }
}
