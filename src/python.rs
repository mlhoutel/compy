use rustpython_parser::{
    ast::{
        BooleanOperator, Comparison, ComprehensionKind, ConversionFlag, ExpressionType, Located,
        Number, Operator, Program, StatementType, StringGroup, UnaryOperator,
    },
    parser,
};
use std::iter;
use std::string::String;

pub fn parse(source: &str) -> Program {
    return parser::parse_program(source).ok().unwrap();
}

pub fn serialize(ast: Program) -> String {
    ast.statements
        .iter()
        .map(|statement| serialize_statement(&statement.node))
        .collect::<Vec<String>>()
        .join("\n")
}

pub fn serialize_inlined(ast: Program) -> String {
    ast.statements
        .iter()
        .map(|statement| serialize_statement(&statement.node))
        .collect::<Vec<String>>()
        .concat()
}

pub fn serialize_statement(statement: &StatementType) -> String {
    let mut text = "".to_string();

    match statement {
        StatementType::Break => text += "break",
        StatementType::Continue => text += "continue",
        StatementType::Return { value } => match value {
            Some(value) => text += &format!("return {}", serialize_expression(&value.node)),
            None => text += "return",
        },
        StatementType::Import { names } => {
            text += &format!(
                "import {}",
                &names
                    .iter()
                    .map(|name| match &name.alias {
                        Some(alias) => format!("{} as {}", name.symbol, alias),
                        None => name.symbol.clone(),
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            );
        }
        StatementType::ImportFrom {
            module,
            names,
            level: _,
        } => {
            if let Some(module) = module {
                text += &format!("from {} ", module)
            }

            text += &format!(
                "import {}",
                &names
                    .iter()
                    .map(|name| match &name.alias {
                        Some(alias) => format!("{} as {}", name.symbol, alias),
                        None => name.symbol.clone(),
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            );
        }
        StatementType::Pass => text += "pass",
        StatementType::Assert { test, msg } => {
            text += &format!("assert {}", serialize_expression(&test.node));

            if let Some(msg) = msg {
                text += &format!(", {}", serialize_expression(&msg.node))
            }
        }
        StatementType::Delete { targets } => {
            text += &format!(
                "del {}",
                &targets
                    .iter()
                    .map(|target| serialize_expression(&target.node))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        StatementType::Assign { targets, value } => {
            let targets_text = targets
                .iter()
                .map(|target| serialize_expression(&target.node))
                .collect::<Vec<String>>()
                .join(" = ");
            let value_text = serialize_expression(&value.node);
            text += &format!("{} = {}", &targets_text, &value_text);
        }
        StatementType::AugAssign { target, op, value } => match op {
            Operator::Add => {
                text += &format!(
                    "{} += {}",
                    serialize_expression(&target.node),
                    serialize_expression(&value.node)
                )
            }
            Operator::Sub => {
                text += &format!(
                    "{} -= {}",
                    serialize_expression(&target.node),
                    serialize_expression(&value.node)
                )
            }
            Operator::Mult => {
                text += &format!(
                    "{} *= {}",
                    serialize_expression(&target.node),
                    serialize_expression(&value.node)
                )
            }
            Operator::MatMult => {
                text += &format!(
                    "{} @= {}",
                    serialize_expression(&target.node),
                    serialize_expression(&value.node)
                )
            }
            Operator::Div => {
                text += &format!(
                    "{} /= {}",
                    serialize_expression(&target.node),
                    serialize_expression(&value.node)
                )
            }
            Operator::Mod => {
                text += &format!(
                    "{} %= {}",
                    serialize_expression(&target.node),
                    serialize_expression(&value.node)
                )
            }
            Operator::Pow => {
                text += &format!(
                    "{} **= {}",
                    serialize_expression(&target.node),
                    serialize_expression(&value.node)
                )
            }
            Operator::LShift => {
                text += &format!(
                    "{} <<= {}",
                    serialize_expression(&target.node),
                    serialize_expression(&value.node)
                )
            }
            Operator::RShift => {
                text += &format!(
                    "{} >>= {}",
                    serialize_expression(&target.node),
                    serialize_expression(&value.node)
                )
            }
            Operator::BitOr => {
                text += &format!(
                    "{} |= {}",
                    serialize_expression(&target.node),
                    serialize_expression(&value.node)
                )
            }
            Operator::BitXor => {
                text += &format!(
                    "{} ^= {}",
                    serialize_expression(&target.node),
                    serialize_expression(&value.node)
                )
            }
            Operator::BitAnd => {
                text += &format!(
                    "{} &= {}",
                    serialize_expression(&target.node),
                    serialize_expression(&value.node)
                )
            }
            Operator::FloorDiv => {
                text += &format!(
                    "{} //= {}",
                    serialize_expression(&target.node),
                    serialize_expression(&value.node)
                )
            }
        },
        StatementType::AnnAssign {
            target,
            annotation,
            value,
        } => match value {
            Some(value) => {
                text += &format!(
                    "{}: {} = {}",
                    serialize_expression(&target.node),
                    serialize_expression(&annotation.node),
                    serialize_expression(&value.node)
                )
            }
            None => {
                text += &format!(
                    "{}: {}",
                    serialize_expression(&target.node),
                    serialize_expression(&annotation.node)
                )
            }
        },
        StatementType::Expression { expression } => text += &serialize_expression(&expression.node),
        StatementType::Global { names } => text += &format!("del {}", &names.join(", ")),
        StatementType::Nonlocal { names } => text += &format!("nonlocal {}", &names.join(", ")),
        StatementType::If { test, body, orelse } => {
            text += &format!(
                "if {}:\n\t{}",
                serialize_expression(&test.node),
                body.iter()
                    .map(|lin| serialize_statement(&lin.node).replace("\n", "\n\t"))
                    .collect::<Vec<String>>()
                    .join("\n\t")
            );

            if let Some(orelse) = orelse {
                text += &format!(
                    "\nelse:\n\t{}",
                    orelse
                        .iter()
                        .map(|lin| serialize_statement(&lin.node).replace("\n", "\n\t"))
                        .collect::<Vec<String>>()
                        .join("\n\t")
                )
            }
        }
        StatementType::While { test, body, orelse } => {
            text += &format!(
                "while {}:\n\t{}",
                serialize_expression(&test.node),
                body.iter()
                    .map(|lin| serialize_statement(&lin.node).replace("\n", "\n\t"))
                    .collect::<Vec<String>>()
                    .join("\n\t")
            );

            if let Some(orelse) = orelse {
                text += &format!(
                    "\nelse:\n\t{}",
                    orelse
                        .iter()
                        .map(|lin| serialize_statement(&lin.node).replace("\n", "\n\t"))
                        .collect::<Vec<String>>()
                        .join("\n\t")
                )
            }
        }
        StatementType::With {
            is_async,
            items,
            body,
        } => {
            if *is_async {
                text += "async "
            }

            text += &format!(
                "with {}:\n\t{}",
                items
                    .iter()
                    .map(|item| {
                        let mut item_text = serialize_expression(&item.context_expr.node);

                        if let Some(optional) = &item.optional_vars {
                            item_text += &format!(" as {}", serialize_expression(&optional.node));
                        }

                        item_text
                    })
                    .collect::<Vec<String>>()
                    .join(", "),
                body.iter()
                    .map(|lin| serialize_statement(&lin.node).replace("\n", "\n\t"))
                    .collect::<Vec<String>>()
                    .join("\n\t")
            );
        }
        StatementType::For {
            is_async,
            target,
            iter,
            body,
            orelse,
        } => {
            if *is_async {
                text += "async "
            }

            text += &format!(
                "for {} in {}:\n\t{}",
                serialize_expression(&target.node),
                serialize_expression(&iter.node),
                body.iter()
                    .map(|lin| serialize_statement(&lin.node).replace("\n", "\n\t"))
                    .collect::<Vec<String>>()
                    .join("\n\t")
            );

            if let Some(orelse) = orelse {
                text += &format!(
                    "\nelse:\n\t{}",
                    orelse
                        .iter()
                        .map(|lin| serialize_statement(&lin.node).replace("\n", "\n\t"))
                        .collect::<Vec<String>>()
                        .join("\n\t"),
                )
            }
        }
        StatementType::Raise { exception, cause } => {
            text += "raise";

            if let Some(exception) = exception {
                text += &format!(" {}(", serialize_expression(&exception.node));

                if let Some(cause) = cause {
                    text += &serialize_expression(&cause.node)
                }

                text += ")";
            }
        }
        StatementType::Try {
            body: _,
            handlers: _,
            orelse: _,
            finalbody: _,
        } => todo!(),
        StatementType::ClassDef {
            name,
            body,
            bases,
            keywords: _,
            decorator_list,
        } => {
            if decorator_list.len() > 0 {
                text += &decorator_list
                    .iter()
                    .map(|decorator| format!("@{}\n", serialize_expression(&decorator.node)))
                    .collect::<Vec<String>>()
                    .concat();
            }

            text += &format!(
                "class {}({}):\n\t{}",
                name,
                bases
                    .iter()
                    .map(|base| serialize_expression(&base.node))
                    .collect::<Vec<String>>()
                    .join(", "),
                body.iter()
                    .map(|lin| serialize_statement(&lin.node).replace("\n", "\n\t"))
                    .collect::<Vec<String>>()
                    .join("\n\t")
            );
        }
        StatementType::FunctionDef {
            is_async,
            name,
            args,
            body,
            decorator_list,
            returns,
        } => {
            if decorator_list.len() > 0 {
                text += &decorator_list
                    .iter()
                    .map(|decorator| format!("@{}\n", serialize_expression(&decorator.node)))
                    .collect::<Vec<String>>()
                    .concat();
            }

            if *is_async {
                text += "async ";
            }

            text += "def ";

            text += &format!(
                "{}({})",
                &name,
                args.args
                    .iter()
                    .map(|arg| match &arg.annotation {
                        Some(annotation) =>
                            format!("{}: {}", arg.arg, serialize_expression(&annotation.node)),
                        None => arg.arg.to_string(),
                    })
                    .collect::<Vec<String>>()
                    .join(", "),
            );

            match returns {
                Some(returns) => {
                    text += &format!(" -> {}:\n\t", serialize_expression(&returns.node))
                }
                None => text += ":\n\t",
            };

            text += &body
                .iter()
                .map(|lin| serialize_statement(&lin.node).replace("\n", "\n\t"))
                .collect::<Vec<String>>()
                .join("\n\t")
        }
    };

    text
}

pub fn serialize_expression(expression: &ExpressionType) -> String {
    let mut text = "".to_string();

    match expression {
        ExpressionType::BoolOp { op, values } => match op {
            BooleanOperator::And => {
                text += &values
                    .iter()
                    .map(|target| serialize_expression(&target.node))
                    .collect::<Vec<String>>()
                    .join(" and ")
            }
            BooleanOperator::Or => {
                text += &values
                    .iter()
                    .map(|target| serialize_expression(&target.node))
                    .collect::<Vec<String>>()
                    .join(" or ")
            }
        },
        ExpressionType::Binop { a, op, b } => match op {
            Operator::Add => {
                text += &format!(
                    "{} + {}",
                    serialize_expression(&a.node),
                    serialize_expression(&b.node)
                )
            }
            Operator::Sub => {
                text += &format!(
                    "{} - {}",
                    serialize_expression(&a.node),
                    serialize_expression(&b.node)
                )
            }
            Operator::Mult => {
                text += &format!(
                    "{} * {}",
                    serialize_expression(&a.node),
                    serialize_expression(&b.node)
                )
            }
            Operator::MatMult => {
                text += &format!(
                    "{} @ {}",
                    serialize_expression(&a.node),
                    serialize_expression(&b.node)
                )
            }
            Operator::Div => {
                text += &format!(
                    "{} / {}",
                    serialize_expression(&a.node),
                    serialize_expression(&b.node)
                )
            }
            Operator::Mod => {
                text += &format!(
                    "{} % {}",
                    serialize_expression(&a.node),
                    serialize_expression(&b.node)
                )
            }
            Operator::Pow => {
                text += &format!(
                    "{} ** {}",
                    serialize_expression(&a.node),
                    serialize_expression(&b.node)
                )
            }
            Operator::LShift => {
                text += &format!(
                    "{} << {}",
                    serialize_expression(&a.node),
                    serialize_expression(&b.node)
                )
            }
            Operator::RShift => {
                text += &format!(
                    "{} >> {}",
                    serialize_expression(&a.node),
                    serialize_expression(&b.node)
                )
            }
            Operator::BitOr => {
                text += &format!(
                    "{} | {}",
                    serialize_expression(&a.node),
                    serialize_expression(&b.node)
                )
            }
            Operator::BitXor => {
                text += &format!(
                    "{} ^ {}",
                    serialize_expression(&a.node),
                    serialize_expression(&b.node)
                )
            }
            Operator::BitAnd => {
                text += &format!(
                    "{} & {}",
                    serialize_expression(&a.node),
                    serialize_expression(&b.node)
                )
            }
            Operator::FloorDiv => {
                text += &format!(
                    "{} // {}",
                    serialize_expression(&a.node),
                    serialize_expression(&b.node)
                )
            }
        },
        ExpressionType::Subscript { a, b } => {
            text += &format!(
                "{}[{}]",
                serialize_expression(&a.node),
                serialize_expression(&b.node)
            )
        }
        ExpressionType::Unop { op, a } => {
            text += &format!(
                "{}{}",
                match op {
                    UnaryOperator::Pos => "+",
                    UnaryOperator::Neg => "-",
                    UnaryOperator::Not => "!",
                    UnaryOperator::Inv => "~",
                },
                serialize_expression(&a.node)
            )
        }
        ExpressionType::Await { value } => {
            text += &format!("await {}", serialize_expression(&value.node))
        }
        ExpressionType::Yield { value } => match value {
            Some(value) => text += &format!("yield {}", serialize_expression(&value.node)),
            None => text += "yield",
        },
        ExpressionType::YieldFrom { value } => {
            text += &format!("yield from {}", serialize_expression(&value.node))
        }
        ExpressionType::Compare { vals, ops } => {
            let vals_iter = vals.iter().map(|val| serialize_expression(&val.node));
            let ops_iter = ops
                .iter()
                .map(|op| match op {
                    Comparison::Equal => "==".to_string(),
                    Comparison::NotEqual => "!=".to_string(),
                    Comparison::Less => "<".to_string(),
                    Comparison::LessOrEqual => "<=".to_string(),
                    Comparison::Greater => ">".to_string(),
                    Comparison::GreaterOrEqual => ">=".to_string(),
                    Comparison::In => "in".to_string(),
                    Comparison::NotIn => "not in".to_string(),
                    Comparison::Is => "is".to_string(),
                    Comparison::IsNot => "not is".to_string(),
                })
                .chain(
                    iter::repeat("".to_string())
                        .take(vals_iter.len() - ops.len())
                        .collect::<Vec<String>>(),
                );

            text += &vals_iter
                .clone()
                .zip(ops_iter.clone())
                .flat_map(|(x, y)| vec![x, y.to_string()])
                .filter(|x| x.len() != 0)
                .collect::<Vec<String>>()
                .join(" ");
        }
        ExpressionType::Attribute { value, name } => {
            text += &format!("{}.{}", serialize_expression(&value.node), name)
        }
        ExpressionType::Call {
            function,
            args,
            keywords,
        } => {
            let mut args_text = args
                .iter()
                .map(|arg| serialize_expression(&arg.node))
                .collect::<Vec<String>>();

            let mut args_keywords = keywords
                .iter()
                .map(|keyword| match &keyword.name {
                    Some(name) => {
                        format!("{} = {}", name, serialize_expression(&keyword.value.node))
                    }
                    None => serialize_expression(&keyword.value.node),
                })
                .collect::<Vec<String>>();

            args_text.append(&mut args_keywords);

            text += &format!(
                "{}({})",
                serialize_expression(&function.node),
                args_text.join(", ")
            )
        }
        ExpressionType::Number { value } => match value {
            Number::Integer { value } => text += &value.to_string(),
            Number::Float { value } => text += &value.to_string(),
            Number::Complex { real, imag } => {
                text += &format!("{}+{}j", &real.to_string(), &imag.to_string())
            }
        },
        ExpressionType::List { elements } => {
            text += &format!(
                "[{}]",
                elements
                    .iter()
                    .map(|element| serialize_expression(&element.node))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        ExpressionType::Tuple { elements } => {
            text += &format!(
                "({}",
                elements
                    .iter()
                    .map(|element| serialize_expression(&element.node))
                    .collect::<Vec<String>>()
                    .join(", ")
            );

            if elements.len() == 1 {
                text += ",";
            }

            text += ")";
        }
        ExpressionType::Dict { elements } => {
            text += &format!(
                "{{ {} }}",
                elements
                    .iter()
                    .map(|(key, value)| {
                        let mut element_text = "".to_string();
                        if let Some(key) = key {
                            element_text += &format!("{}: ", serialize_expression(&key.node))
                        }
                        element_text += &serialize_expression(&value.node);

                        element_text
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            );
        }
        ExpressionType::Set { elements } => {
            text += &format!(
                "{{ {} }}",
                elements
                    .iter()
                    .map(|elem| serialize_expression(&elem.node))
                    .collect::<Vec<String>>()
                    .join(", ")
            );
        }
        ExpressionType::Comprehension { kind, generators } => {
            let (before, after) = match kind.as_ref().clone() {
                ComprehensionKind::GeneratorExpression { element } => {
                    (format!("({}", serialize_expression(&element.node)), ")")
                }
                ComprehensionKind::List { element } => {
                    (format!("[{}", serialize_expression(&element.node)), "]")
                }
                ComprehensionKind::Set { element } => {
                    (format!("{{{}", serialize_expression(&element.node)), "}")
                }
                ComprehensionKind::Dict { key, value } => (
                    format!(
                        "{{{}: {}",
                        serialize_expression(&key.node),
                        serialize_expression(&value.node)
                    ),
                    "}",
                ),
            };

            text += &format!(
                "{}{}{}",
                before,
                generators
                    .iter()
                    .map(|comp| {
                        let mut comp_text = "".to_string();

                        if comp.is_async {
                            comp_text += " async";
                        }

                        comp_text += &format!(
                            " for {} in {}",
                            serialize_expression(&comp.target.node),
                            serialize_expression(&comp.iter.node)
                        );

                        if comp.ifs.len() > 0 {
                            comp_text += &format!(
                                " if {}",
                                &comp
                                    .ifs
                                    .iter()
                                    .map(|i| serialize_expression(&i.node))
                                    .collect::<Vec<String>>()
                                    .join(" ")
                            );
                        }

                        comp_text
                    })
                    .collect::<Vec<String>>()
                    .join(", "),
                after
            );
        }
        ExpressionType::Starred { value } => {
            text += &format!("*{}", &serialize_expression(&value.node))
        }
        ExpressionType::Slice { elements } => {
            let slices = &elements
                .iter()
                .filter(|element| match &element.node {
                    ExpressionType::None => false,
                    _ => true,
                })
                .collect::<Vec<&Located<ExpressionType>>>();

            text += &slices
                .iter()
                .map(|element| serialize_expression(&element.node))
                .collect::<Vec<String>>()
                .join(":");

            if slices.len() < 3 {
                text += ":";
            }
        }
        ExpressionType::String { value } => {
            fn handler(value: &StringGroup) -> String {
                match value {
                    StringGroup::Constant { value } => {
                        format!(
                            "'{}'",
                            value
                                .replace("\n", "\\n")
                                .replace("\r", "\\r")
                                .replace("\t", "\\t")
                        )
                    }
                    StringGroup::FormattedValue {
                        value,
                        conversion,
                        spec,
                    } => {
                        fn formatted_handler(
                            value: &Box<Located<ExpressionType>>,
                            conversion: &Option<ConversionFlag>,
                            spec: &Option<Box<StringGroup>>,
                        ) -> String {
                            let mut text = "".to_string();

                            let conv = if let Some(conversion) = conversion {
                                match conversion {
                                    ConversionFlag::Str => "",
                                    ConversionFlag::Ascii => "!a",
                                    ConversionFlag::Repr => "!r",
                                }
                            } else {
                                ""
                            };

                            text += &format!("{{{}{}}}", serialize_expression(&value.node), conv);

                            if let Some(spec) = spec {
                                text += &handler(spec)
                            };

                            text
                        }

                        format!("f'{}'", formatted_handler(value, conversion, spec))
                    }
                    StringGroup::Joined { values } => {
                        let mut formatted = false;

                        let values = values
                            .iter()
                            .map(|value| {
                                let mut str = handler(value);

                                if str.starts_with("f'") {
                                    formatted = true;
                                    str = str[2..].to_string();
                                } else if str.starts_with("'") {
                                    formatted = true;
                                    str = str[1..].to_string();
                                }

                                if str.ends_with("'") {
                                    formatted = true;
                                    str = str[..str.len() - 1].to_string();
                                }

                                str
                            })
                            .collect::<Vec<String>>()
                            .concat();

                        format!("{}'''{}'''", if formatted { "f" } else { "" }, values)
                    }
                }
            }

            text += &handler(value);
        }
        ExpressionType::Bytes { value } => {
            text += &format!(
                "b'{}'",
                value
                    .iter()
                    .map(|value| format!("\\x{:x?}", value))
                    .collect::<Vec<String>>()
                    .concat()
            )
        }
        ExpressionType::Identifier { name } => text += name,
        ExpressionType::Lambda { args, body } => {
            text += "(lambda";

            if args.args.len() > 0 {
                text = text
                    + " "
                    + &args
                        .args
                        .iter()
                        .map(|arg| match &arg.annotation {
                            Some(_) => panic!("lambda's syntax does not support annotation"),
                            None => arg.arg.to_string(),
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
            }

            text += &format!(": {})", serialize_expression(&body.node))
        }
        ExpressionType::IfExpression { test, body, orelse } => {
            text += &format!(
                "{} if {}",
                serialize_expression(&body.node),
                serialize_expression(&test.node)
            );

            text += &format!(" else {}", serialize_expression(&orelse.node));
        }
        ExpressionType::NamedExpression { left, right } => {
            text += &format!(
                "{} := {}",
                serialize_expression(&left.node),
                serialize_expression(&right.node)
            );
        }
        ExpressionType::True => text += "True",
        ExpressionType::False => text += "False",
        ExpressionType::None => text += "None",
        ExpressionType::Ellipsis => text += "...",
    }

    text
}

#[cfg(test)]
mod tests {
    use crate::python::{parse, serialize};

    #[test]
    fn integer_variable_declaration() {
        let source = "a = 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn floating_variable_declaration() {
        let source = "a = 1.57";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn multi_variable_declaration() {
        let source = "a = b = 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn tuple_variable_declaration() {
        let source = "(a, b) = c";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn augmented_additive_assign() {
        let source = "a += 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn augmented_substractive_assign() {
        let source = "a -= 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn augmented_multiply_assign() {
        let source = "a *= 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn augmented_divide_assign() {
        let source = "a /= 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn annotated_integer_assign() {
        let source = "a: int = 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn annotated_empty_assign() {
        let source = "a: int";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn integer_additive_operation() {
        let source = "2 + 3";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn float_additive_operation() {
        let source = "2.5 + 3.6";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn integer_substractive_operation() {
        let source = "9 - 45";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn integer_multiply_operation() {
        let source = "2 * 60";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn integer_divide_operation() {
        let source = "90 / 5";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn integer_operation_declaration() {
        let source = "a = 1 + 2";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn reference_operation_declaration() {
        let source = "c = a + b";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn and_boolean_operation() {
        let source = "a and b";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn or_boolean_operation() {
        let source = "a or b";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn composite_boolean_operation() {
        let source = "a and b and c or d";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn break_operation() {
        let source = "break";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn continue_operation() {
        let source = "continue";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn return_empty() {
        let source = "return";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn return_integer() {
        let source = "return 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn import_reference() {
        let source = "import a";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn import_alias() {
        let source = "import a as b";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn if_boolean() {
        let source = "if a and b:\n\tc = 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn if_else_boolean() {
        let source = "if a and b:\n\tc = 1\nelse:\n\tc = 2";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn if_multi_line() {
        let source = "if a and b:\n\tc = 1\n\td = 2";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn compare_values() {
        let source = "a > 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn while_condition() {
        let source = "while a > 1:\n\tc = 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn while_boolean() {
        let source = "while True:\n\tc = 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn while_indent_body() {
        let source = "while True:\n\twhile False:\n\t\tc = 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn for_int_range() {
        let source = "for i in range(10):\n\tprint(i)";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn for_multiline() {
        let source = "for i in range(10):\n\ta = i * 2\n\tprint(a)";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn for_indent_body() {
        let source = "for x in width:\n\tfor y in heigth:\n\t\tprint((x, y))";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn function_params() {
        let source = "def add(a, b):\n\treturn a + b";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn function_params_typed() {
        let source = "def add(a: int, b: int) -> int:\n\treturn a + b";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn function_multiline() {
        let source = "def test():\n\tc = 1\n\td = 2";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn function_indent_body() {
        let source = "def test():\n\tfor i in range(10):\n\t\tprint(i)";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn lambda_no_params() {
        let source = "(lambda: print(a))";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn lambda_params() {
        let source = "(lambda a: print(a))";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn import_module() {
        let source = "import a";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn import_module_alias() {
        let source = "import a as b";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn import_module_multi() {
        let source = "import a as b, c, d as e";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn import_from_module() {
        let source = "from a import b";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn import_from_module_multi() {
        let source = "from a import b, c";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn class_definition() {
        let source = "class A():\n\ta = 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn class_definition_inheritance() {
        let source = "class A(B):\n\ta = 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn class_definition_decorator() {
        let source = "@test\nclass A(B):\n\ta = 1";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn class_definition_body_multi() {
        let source = "class A(B):\n\ta = 1\n\tname = 'test'";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn class_definition_body_indented() {
        let source = "class A(B):\n\tdef __init__(self, a):\n\t\tself.a = a";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn subscript_operator() {
        let source = "a[b]";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn dict_type() {
        let source = "{ 'a': 1, 'b': 2 }";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn set_type() {
        let source = "{ 'a', 'b' }";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn comprehension_list() {
        let source = "[i * 2 for i in l]";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn comprehension_list_if() {
        let source = "[i * 2 for i in l if i % 2 == 0]";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn comprehension_set() {
        let source = "{i * 2 for i in l}";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn comprehension_dict() {
        let source = "{k: i * 2 for i in l}";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn comprehension_gen() {
        let source = "(i * 2 for i in l)";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn if_else_expression() {
        let source = "a if True else b";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn string_expression() {
        let source = "'string'";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn string_expression_lines() {
        let source = "'\\nstring'";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn string_byte_array() {
        let source = "b'\\x68\\x65\\x6c\\x6c\\x6f'";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn formatted_string() {
        let source = "f'{s}'";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn formatted_string_ascii() {
        let source = "f'{s!a}'";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn formatted_string_repr() {
        let source = "f'{s!r}'";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn formatted_string_multi() {
        let source = "f'''{1} then {2} then {3}'''";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn formatted_string_multiline_underscript() {
        let source = "f'''\\n{a['b']}\\n'''";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn tuple_empty() {
        let source = "()";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn tuple_elements() {
        let source = "(a, b, c)";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn tuple_once() {
        let source = "(a,)";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn list_elements() {
        let source = "[1, 2, 3, 4]";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn list_slice_empty_step() {
        let source = "a[:]";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn list_slice_start() {
        let source = "a[1:]";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn list_slice_start_step() {
        let source = "a[1:1:]";
        assert_eq!(serialize(parse(source)), source)
    }

    #[test]
    fn list_slice_start_step_end() {
        let source = "a[1:1:1]";
        assert_eq!(serialize(parse(source)), source)
    }
}
