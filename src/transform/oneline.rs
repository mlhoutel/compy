use crate::{python::parse, serialize_statement};
use num_bigint::BigInt;
use rustpython_parser::{ast::{
    BooleanOperator, Comparison, Comprehension, ComprehensionKind, ConversionFlag, ExpressionType,
    ImportSymbol, Keyword, Located, Location, Number, Operator, Parameter, Parameters, Program,
    StatementType, StringGroup, UnaryOperator, Varargs,
}, parser};
use std::collections::{HashSet, VecDeque};

use super::super::python::serialize_expression;

const CORE_NAME: &str = "INL__CORE";
const TEMP_NAME: &str = "INL__TEMP";
const SPLIT_NAME: &str = "INL__SPLIT";
const STATE_NAME: &str = "INL__STATE";
const IMPORT_KEY: &str = "INL__IMPORT_";
const ITER_NAME: &str = "INL__ITER";
const ITER_CURSOR: &str = "INL__ITCUR";
const STOP_COND: &str = "INL__STOP";
const VARS_NAME: &str = "vars";
const HASATTR_NAME: &str = "hasattr";
const GLOBALS_NAME: &str = "globals";
const UPDATE_NAME: &str = "update";
const SLICE_NAME: &str = "slice";
const EMPTY_NAME: &str = "_";
const THROW_NAME: &str = "throw";
const SYSTEM_NAME: &str = "sys";
const TOITER_NAME: &str = "iter";
const NEXT_NAME: &str = "next";
const CTYPE_NAME: &str = "ctypes";
const INSPECT_NAME: &str = "inspect";
const EXCEPTHOOK_NAME: &str = "excepthook";
const BUILTINS_NAME: &str = "__builtins__";
const SETATTR_NAME: &str = "setattr";
const DELATTR_NAME: &str = "delattr";
const SETITEM_NAME: &str = "__setitem__";
const DELITEM_NAME: &str = "__delitem__";
const TEMP_EXCEPTHOOK: &str = "INL__EXCEPTHOOK";
const EXCEPTION_DEF_TYPE: &str = "RuntimeError";
const EXCEPTION_DEF_MSG: &str = "No active exception to reraise";


/// Transform a python program to a one-liner python program
///
/// > **Manual state handling:**
/// >
/// > we assume that all code in a branch after a return, pass, or continue statement can be eliminated.
/// > we also assume that if a return statement happens in a conditional statement, all remaining code will execute in the else block.
/// >
/// > as such, we can handle the execution flow in the state variable
/// >  * [0] Return value buffer
/// >  * [1] Target indentation
/// >
/// > We can then define this state in each function and add checks in conditionals to iterate towards the target indentation.
/// > 
/// > (i): this may add extra code and checks, which could potentially affect program performance.
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

/// Reset the state with base values (None, 1) at the beggining of a new body
fn reset_state() -> ExpressionType {
    update_state(ExpressionType::None, 1)
}

/// Update the current state with the value and new target indentation, for instance:
/// 
/// `__INL__STATE := (None, 1)`
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

/// Extract the current indentation from the python state, for instance:
/// 
/// `__INL__STATE[1]`
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

/// Compare python state with current target, for instance:
/// 
/// `__INL__STATE[1] > 1`
fn check_state_target(target: u32) -> ExpressionType {
    check_state_target_operator(target, Comparison::GreaterOrEqual)
}

/// Compare python state with current target with a custom operator
fn check_state_target_operator(target: u32, comparison: Comparison) -> ExpressionType {
    ExpressionType::Compare {
        vals: vec![
            to_located(state_target()),
            to_located(ExpressionType::Number {
                value: Number::Integer {
                    value: BigInt::from(target),
                },
            }),
        ],
        ops: vec![comparison],
    }
}

/// Explicitely infer an argument source, for instance:
/// 
/// `print if 'print' in vars() else __builtins__.print if hasattr(__builtins__, 'print') else None)`
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

/// Explicitely infer an output source for scoped updates, for instance:
/// 
/// `[vars().__setitem__("next", __INL__TEMP[3])] if "next" in vars() else None,`
fn optional_update(identifier: String, value: ExpressionType) -> ExpressionType {
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
        body: Box::from(to_located(ExpressionType::Call {
            function: Box::from(to_located(ExpressionType::Attribute {
                value: Box::from(to_located(ExpressionType::Call {
                    function: Box::from(to_located(ExpressionType::Identifier {
                        name: VARS_NAME.to_string(),
                    })),
                    args: vec![],
                    keywords: vec![],
                })),
                name: SETITEM_NAME.to_string()
            })),
            args: vec![
                to_located(ExpressionType::String {
                    value: StringGroup::Constant {
                        value: identifier.to_string(),
                    },
                }),
                to_located(value),
            ],
            keywords: vec![]
        })),
        orelse: Box::from(to_located(ExpressionType::None)) 
    }
}

/// Convert an expression to a statement
fn to_statement(expression: ExpressionType) -> StatementType {
    return StatementType::Expression {
        expression: to_located(expression),
    };
}

/// Make an list expression with the given elements subscripted at id [-1] 
/// for returning the last value inside the list
/// 
/// ex: [x, return_value][-1]
fn subscripted_list_return(elements: Vec<Located<ExpressionType>>) -> ExpressionType {
    ExpressionType::Subscript { 
        a: Box::from(to_located(ExpressionType::List { 
            elements,
        })), 
        b: Box::from(to_located(ExpressionType::Number {
            value: Number::Integer {
                value: BigInt::from(-1),
            },
        })),
    }
}

/// Move each import at the top of the program, they will stay as Statements
/// and will be inlined with ";" separators. To keep the potential scoping of the import,
/// we will prepend the moved global import with an identifier, and re-declare their
/// true identifier with the prepended version at the time they are truly imported.
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

/// Prepare a body with a given level and from a certain point
/// for flow management (handle program branchings and flow 
/// statement like return, break, pass, continue)
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
        // if there still is some code after the branching
        if split_flow > 0 && (i + 1) < body.len() {
            let flow_target = level + split_flow - 1; // to account for split_flow that start at 1
            
            let check_state = ExpressionType::IfExpression {
                test: Box::from(to_located(check_state_target(level + 1))),
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

/// Convert an assign to an one-liner compatible one
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

/// Convert an assign list to a list of assignments
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

/// Convert a statement to a one-liner expression with a given indentation
/// level for flow management
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

            let lambda_body = to_located(subscripted_list_return(lambda_statements));

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
            is_async: _,
            target,
            iter,
            body,
            orelse,
        } => {
            // Transform the python loop to a While statement before inlining it
            // 
            // for (i in range(10)):
            //          <=>
            // it = iter(range(10))
            // cur = next(it, "STOP") 
            // while cur != "STOP":
            //      i = cur
            //      cur = next(it, "STOP") 
            // `it = iter(range(10))`


            // We need to explicitly add the current indentation for the iterator and the cursor
            // to avoid sharing it with lower loops when several for loops are nested.
            let local_iter_name = ITER_NAME.to_string() + "__" + level.to_string().as_str();
            let local_cursor_name = ITER_CURSOR.to_string() + "__" + level.to_string().as_str();

            let iter_assign = ExpressionType::NamedExpression { 
                left: Box::from(to_located(ExpressionType::Identifier { 
                    name: local_iter_name.to_string()
                })), 
                right: Box::from(to_located(ExpressionType::Call { 
                    function: Box::from(to_located(ExpressionType::Identifier { 
                        name: TOITER_NAME.to_string() 
                    })),
                    args: vec![to_located(clone_expression(&iter.node))], 
                    keywords: vec![] 
                })),
            };

            // `cur = next(it, "STOP")`
            let cur_assign = ExpressionType::NamedExpression { 
                left: Box::from(to_located(ExpressionType::Identifier { 
                    name: local_cursor_name.to_string(),
                })), 
                right: Box::from(to_located(ExpressionType::Call { 
                    function: Box::from(to_located(ExpressionType::Identifier {
                            name: NEXT_NAME.to_string() 
                    })), 
                    args: vec![
                        to_located(ExpressionType::Identifier { 
                            name: local_iter_name.to_string()
                        }),
                        to_located(ExpressionType::String { value:
                            StringGroup::Constant { 
                                value: STOP_COND.to_string()  
                            } 
                        })
                    ], 
                    keywords: vec![], 
                })),
            };

            let mut new_body = vec![];

            // To account for case where there is tuple unpacking, we temporarly
            // assign the iterator value to a cursor variable, then unpack and 
            // assign it at the beggining of the while body.
            // `i = cur`
            new_body.push(to_located(StatementType::Assign { 
                targets: vec![to_located(clone_expression(&target.node))], 
                value: to_located(ExpressionType::Identifier { 
                    name: local_cursor_name.to_string(),
                }) 
            }));
            
            // `cur = next(it, "STOP")`
            new_body.push(to_located(StatementType::Assign { 
                targets: vec![
                    to_located(ExpressionType::Identifier { 
                        name: local_cursor_name.to_string(),
                    }) 
                ], 
                value: to_located(ExpressionType::Call { 
                    function: Box::from(to_located(ExpressionType::Identifier {
                            name: NEXT_NAME.to_string() 
                    })), 
                    args: vec![
                        to_located(ExpressionType::Identifier { 
                            name: local_iter_name.to_string() 
                        }),
                        to_located(ExpressionType::String { value:
                            StringGroup::Constant { 
                                value: STOP_COND.to_string()  
                            } 
                        })
                    ], 
                    keywords: vec![], 
                })
            }));

            // inject the body of the for loop into the while loop
            for b in body {
                new_body.append(&mut parser::parse_statement(&serialize_statement(&b.node)).ok().unwrap());
            }

            let mut new_orelse = vec![];

            // inject the or else body of the for loop into the or else while loop
            if let Some(orelse) = orelse {
                for o in orelse {
                    new_orelse.append(&mut parser::parse_statement(&serialize_statement(&o.node)).ok().unwrap());
                }
            }

            // `while cur != "STOP":`
            let while_statement = StatementType::While { 
                test: to_located(ExpressionType::Compare { 
                    vals: vec![
                        to_located(ExpressionType::Identifier { name: local_cursor_name.to_string() }),
                        to_located(ExpressionType::String { value: StringGroup::Constant { value: STOP_COND.to_string() } })
                    ], 
                    ops: vec![Comparison::NotEqual] 
                }), 
                body: new_body,
                orelse: match orelse {
                    Some(_) => Some(new_orelse),
                    None => None,
                }
            };

            ExpressionType::List { 
                elements: vec![
                    to_located(iter_assign), 
                    to_located(cur_assign),
                    to_located(to_expression(&while_statement, level))
                ]
            }
        }
        StatementType::While { test, body, orelse } => {
            
            let mut condition: Located<ExpressionType> = to_located(clone_expression(&test.node));
            let mut variants = HashSet::new();
            let local_level = level + 2;
            let previous_level = level;

            // Find variants in test, add each as a param and replace with local name
            condition.node = clone_expression_adapter(&condition.node, &mut |located| {
                if let ExpressionType::Identifier { name } = located {
                    variants.insert(name.to_string());

                    return ExpressionType::Identifier {
                        name: name.to_string()
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

            let mut body_composite = prepare_body(body, local_level - 1, 0);

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

            let recursive_call = ExpressionType::Call {
                function: Box::from(to_located(ExpressionType::Identifier {
                    name: CORE_NAME.to_string(),
                })),
                args: args
                    .iter()
                    .map(|arg| to_located(ExpressionType::Identifier { name: arg.clone() }))
                    .collect::<Vec<Located<ExpressionType>>>(),
                keywords: vec![],
            };

            body_composite.push(to_located(clone_expression(&recursive_call)));
            
            // TODO: use subscripted_list_return if the adapter is not necessary?
            // Why do we need to use local renamed variables in the first place?
            let lambda_body = ExpressionType::Subscript {
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
                                    name: name.to_string()
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
            };

            let mut core_body = vec![];

            let exports = vec![
                Vec::from_iter(variants.clone()),
                Vec::from_iter(variables.clone()),
            ]
            .concat();

            // Prepare the values for the return startement
            // and local variables updates
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

            // Before executing the body of the current iteration, we need to check that
            // no changes have been made to the program flow in the last iteration body.
            // The flow tree for the loop execution will be the following:
            //
            // ```
            // if target test
            //   if condition test
            //     if target -1 test (continue)
            //       reset loop target
            //       <go to next iter>
            //     else
            //       reset loop target
            //       <play the loop>
            //     end
            //   else
            //     reset to previous 
            //     <return values>
            //   end
            // else
            //   <return values>
            // end
            // ```

            let target_continue_test = subscripted_list_return(
                vec![to_located(ExpressionType::IfExpression {
                test: Box::from(to_located(check_state_target_operator(local_level, Comparison::Less))),
                body: Box::from(to_located(subscripted_list_return(vec![
                    to_located(update_state(ExpressionType::None, local_level)), // reset loop target
                    to_located(clone_expression(&recursive_call)), // <go to next iter>
                ]))),
                orelse: Box::from(to_located(subscripted_list_return(vec![
                        to_located(update_state(ExpressionType::None, local_level)), // reset loop target
                        to_located(lambda_body), // <play the loop>
                ])))
            })]);

            let condition_test = subscripted_list_return(
                vec![to_located(ExpressionType::IfExpression {
                test: Box::from(condition), // if condition test
                body: Box::from(to_located(target_continue_test)), // target continue test
                orelse: Box::from(to_located(subscripted_list_return(vec![
                    to_located(update_state(ExpressionType::None, previous_level)), // reset to previous target
                    to_located(ExpressionType::Tuple { 
                        elements: return_state.iter()
                            .map(|r| to_located(clone_expression(&r.node)))
                            .collect::<Vec<Located<ExpressionType>>>(), // <return values>
                    })
                ]))),
            })]);

            let target_test = subscripted_list_return(
                vec![to_located(ExpressionType::IfExpression {
                test: Box::from(to_located(check_state_target(local_level-1))), // if target test or continue
                body: Box::from(to_located(condition_test)), // condition_test,
                orelse: Box::from(
                    to_located(ExpressionType::Tuple { elements: return_state }) // <return values>
                )
            })]);

            core_body.push(to_located(target_test));

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
                body: Box::from(to_located(subscripted_list_return(core_body)))
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
                call_operations.push(to_located(optional_update(
                    variant.to_string(),
                    ExpressionType::Subscript {
                        a: Box::from(to_located(ExpressionType::Identifier {
                            name: TEMP_NAME.to_string(),
                        })),
                        b: Box::from(to_located(ExpressionType::Number {
                            value: Number::Integer {
                                value: BigInt::from(i + 1),
                            },
                        })),
                    }
                )));
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
        StatementType::Continue => update_state(ExpressionType::None, level - 2),
        StatementType::Break => update_state(ExpressionType::None, level - 4),
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

/// Add an empty location to a node
fn to_located<T>(node: T) -> Located<T> {
    Located {
        location: Location::new(0, 0),
        node: node,
    }
}

/// Clone an expression
fn clone_expression(expression: &ExpressionType) -> ExpressionType {
    clone_expression_adapter(expression, &mut |located| located)
}

/// Clone parameters
fn clone_parameters(args: &Parameters) -> Parameters {
    clone_parameters_adapter(args, &mut |located| located)
}

/// Clone an expression and apply a given function to each sub expression found
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

/// Clone parameters and apply a given function to each sub expression found
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
    use std::sync::{Mutex, Arc};

    use crate::python::{parse, serialize_inlined};
    use crate::transform::oneline::oneline;
    use rustpython_vm as rvm;
    use rvm::{extend_class, py_class};

    /// Use interpreter to return the outputs of a given python source code
    fn program_outputs(source: &str) -> String {
        rvm::Interpreter::without_stdlib(Default::default()).enter(|vm| {
            let scope = vm.new_scope_with_builtins();
            let buffer: Arc<Mutex<String>> = Arc::new(Mutex::new("".to_string()));

            let ctx = &vm.ctx;
            let cls = rvm::PyRef::leak(py_class!(
                ctx,
                "Tstdout",
                vm.ctx.types.object_type.to_owned(),
                {}
            ));
            
            let closure_buffer = buffer.clone();

            let write_method = vm.new_method(   
            "write",
            cls,
                move |_self: rvm::PyObjectRef, data: rvm::builtins::PyStrRef, _vm: &rvm::VirtualMachine| -> rvm::PyResult<()> {
                    let mut buffer = closure_buffer.lock().unwrap();
                    *buffer += data.as_str();
                    Result::Ok(())
                },
            );

            let flush_method = vm.new_method(
                "flush", 
                cls, 
                |_self: rvm::PyObjectRef| {}
            );

            extend_class!(ctx, cls, {
                "write" => write_method,
                "flush" => flush_method,
            });

            let stdout = ctx.new_base_object(cls.to_owned(), None);
            vm.sys_module.set_attr("stdout", stdout, vm).unwrap();
            
            match vm
                .compile(source, rvm::compiler::Mode::Exec, "<embedded>".to_owned())
                .map_err(|err| vm.new_syntax_error(&err, Some(source)))
                .and_then(|code_obj| vm.run_code_obj(code_obj, scope.clone())) {
                    Ok(_output) => {            
                        buffer.lock().unwrap().to_string()
                    },
                    Err(err) => {
                        vm.print_exception(err); 
                        buffer.lock().unwrap().to_string()
                    },
                }               
        })
        
    }

    /// Assert that the oneliner python code have the same outputs as the base one
    fn valid_output(source: &str) {
        let serialized = serialize_inlined(oneline(parse(source)));
        println!("{}", serialized);

        let expected = program_outputs(source);
        let oneliner = program_outputs(serialized.as_str());

        assert_eq!(expected, oneliner, "Error: programs stdout are not matching:\n[base]: {expected}\n[oneliner]: {oneliner}\n\nThe oneliner source code is the following one:\n\n{serialized}")
    }

    #[test]
    fn empty_program() {
        let source = r#"
"#;
        valid_output(source);
    }

    #[test]
    fn variable_declaration() {
        let source = r#"
a = 1
print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn variable_declaration_multi() {
        let source = r#"
a = 1
b = a
print(b)
"#;
        valid_output(source);
    }

    #[test]
    fn variable_bin_operation() {
        let source = r#"
a = 1
b = a + 2
print(b)
"#;
        valid_output(source);
    }

    #[test]
    fn function_call() {
        let source = r#"
a = 1
b = a + 2
c = a + b
print(c)
"#;
        valid_output(source);
    }

    #[test]
    fn type_list() {
        let source = r#"
a = [1, 2, 3]
print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn type_tuple() {
        let source = r#"
a = (1, 2, 3)
print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn type_set() {
        let source = r#"
a = { 1, 2, 3 }
print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn type_dict() {
        let source = r#"
a = { 'a': 1, 'b': 2, 'c': 3 }
print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn type_string() {
        let source = r#"
a = 'abc'
print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn if_statement() {
        let source = r#"
if True:
    print(1)
"#;
        valid_output(source);
    }

    #[test]
    fn if_else_statement() {
        let source = r#"
if False:
    print(1)
else:
    print(2)
"#;
        valid_output(source);
    }

    #[test]
    fn list_comprehension() {
        let source = r#"
a = [i for i in range(10)]
print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn lambda_function() {
        let source = r#"
a = (lambda x: x + 1)(1)
print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn function_declaration() {
        let source = r#"
a = 1
def sprint(v):
    print(v)
sprint(a)
"#;
        valid_output(source);
    }

    #[test]
    fn function_scoped_variable() {
        let source = r#"
a = 1

def test():
    a = 2

print(a)
test()
print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn function_reference_variable() {
        let source = r#"
a = 1

def test(a):
    a = 2

print(a)
test(a)
print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn function_declaration_decorated() {
        let source = r#"
a = 1
@property
def sprint(v):
    print(v)
sprint(a)
"#;
        valid_output(source);
    }

    #[test]
    fn function_declaration_recursive() {
        let source =r#"
def fib(i):
    if i <= 1:
        return i
    else:
        return fib(i-1) + fib(i-2)
print(fib(10))
"#;
        valid_output(source);
    }

    #[test]
    fn function_declaration_parameter() {
        let source = r#"
a = 1
def sprint(v):
    print(v)
sprint(a)
"#;
        valid_output(source);
    }

    #[test]
    fn for_loop() {
        let source = r#"
for i in range(10):
    print(i)
"#;
        valid_output(source);
    }

    #[test]
    fn for_loop_else() {
        let source = r#"
for i in range(10):
    print(i)
else:
    print('none')
"#;
        valid_output(source);
    }

    #[test]
    fn for_loop_return() {
        let source = r#"
def main():
    for i in range(10):
        if i == 2:
            return
        print(i)

main()
"#;
        valid_output(source);

    }
    
    #[test]
    fn for_loop_break() {
        let source = r#"
for i in range(10):
    if i == 2:
        break
    print(i)
"#;
        valid_output(source);
    }
    
    #[test]
    fn for_loop_continue() {
        let source = r#"
for i in range(10):
    if i == 2:
        continue
    print(i)
"#;
        valid_output(source);
    }

    #[test]
    fn for_loop_in_for_loop() {
        let source = r#"
for i in range(10):
    for j in range(10):
        print(i, j)
"#;
        valid_output(source);
    }

    #[test]
    fn while_loop() {
        let source = r#"
i = 0
while i < 5:
    i += 1
    print('test')
"#;
        valid_output(source);
    }

    #[test]
    fn while_loop_else() {
        let source = r#"
a = 1
while a < 5:
    a += 1
    print(a)
else:
    print(a + 10)
"#;
        valid_output(source);
    }

    #[test]
    fn while_loop_break() {
        let source = r#"
i = 0
while i < 10:
    if i == 2:
        break
    i += 1
    print(i)
"#;
        valid_output(source);
    }

    #[test]
    fn while_loop_continue() {
        let source = r#"
i = 0
while i < 10:
    i += 1
    if i == 2:
        continue
    print(i)
"#;
        valid_output(source);
    }
    #[test]
    fn while_loop_pass() {
        let source = r#"
i = 0
while i < 10:
    if i == 2:
        pass
    i += 1
    print(i)
"#;
        valid_output(source);
    }

    #[test]
    fn while_loop_return() {
        let source = r#"
def main():
    i = 0
    while i < 10:
        if i == 2:
            return
        i += 1
        print(i)
main()
"#;
        valid_output(source);
    }

    #[test]
    fn while_loop_in_while_loop() {
               let source = r#"
i = 0
while i < 10:
    i += 1
    j = 0
    while j < 10:
        j += 1
        print(i, j)
"#;
        valid_output(source); 
    }

    #[test]
    fn while_loop_in_for_loop() {
               let source = r#"
for i in range(10):
    j = 0
    while j < 10:
        j += 1
        print(i, j)
"#;
        valid_output(source); 
    }

    #[test]
    fn for_loop_in_while_loop() {
               let source = r#"
i = 0
while i < 10:
    i += 1
    for j in range(10):
        print(i, j)
"#;
        valid_output(source); 
    }


    #[test]
    fn class_declaration() {
        let source = r#"
class A():
    m = 'bar'
    def foo(self):
        return self.m
a = A()
print(a.foo())
"#;
        valid_output(source);
    }

    #[test]
    fn remove_after_return() {
        let source = r#"
def main():
    print(1)
    return
    print(2)

main()
"#;
        valid_output(source);
    }

    #[test]
    fn split_after_return() {
        let source = r#"
def main():
    print(1)
    if True:
        return
    print(2)

main()
"#;
        valid_output(source);
    }

    #[test]
    fn function_return() {
        let source = r#"
def a():
    if True:
        return 1
    return 2
print(a())
"#;
        valid_output(source);
    }

    #[test]
    fn optional_variable() {
        let source = r#"
a = 1
b = 2
while a < 5:
    a += 1
    b -= 1
print(a)
print(b)
"#;
        valid_output(source);
    }

    #[test]
    fn optional_function() {
        let source = r#"
a = 1
while a < 5:
    a += 1
    print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn optional_function_scoped() {
        let source = r#"
def main():
    a = 1
    while a < 10:
        a += 1
        print(a)
main()
"#;
        valid_output(source);
    }

    #[test]
    fn import_statement() {
        let source = r#"
import math
print(math.pi)
"#;
        valid_output(source);
    }

    #[test]
    fn global_statement() {
        let source = r#"
a = 1
def main():
    global a
    print(a)

main()
"#;
        valid_output(source);
    }

    #[test]
    fn global_statement_update() {
        let source = r#"
a = 1
def main():
    global a
    a = 2

main()
print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn assign_statement_attribute() {
        let source = r#"
class A():
    pass
a = A()
a.attr = 1
print(a.attr)
"#;
        valid_output(source);
    }

    #[test]
    fn assign_statement_subscript() {
        let source = r#"
a = [0, 1, 2]
a[1] = 1
print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn assign_statement_multi() {
        let source = r#"
a = b = 1
print(a)
print(b)
"#;
        valid_output(source);
    }

    #[test]
    fn assign_statement_tuple() {
        let source = r#"
(a, b) = (1, 2)
print(a)
print(b)
"#;
        valid_output(source);
    }

    #[test]
    fn assign_statement_list() {
        let source = r#"
[a, b] = [1, 2]
print(a)
print(b)
"#;
        valid_output(source);
    }

    #[test]
    fn slice_array() {
        let source = r#"
s = [0,1,2,3]
print(s[:])
"#;
        valid_output(source);
    }

    #[test]
    fn raise_exception() {
        let source = r#"
raise
"#;
        valid_output(source);
    }

    #[test]
    fn raise_exception_message() {
        let source = r#"
raise Exception('error')
"#;
        valid_output(source);
    }

    #[test]
    fn try_except() {
        let source = r#"
try:
    print('test')
except:
    print('error')
"#;
        valid_output(source);
    }

    #[test]
    fn del_variable() {
        let source = r#"
a = 1
del a
print(a)
"#;
        valid_output(source);
    }

    #[test]
    fn del_item() {
        let source = r#"
l = [0,1,2]
del l[0]
print(l)
"#;
        valid_output(source);
    }

    #[test]
    fn del_slice() {
        let source = r#"
l = [0,1,2]
del l[1:2]
print(l)
"#;
        valid_output(source);
    }


}
