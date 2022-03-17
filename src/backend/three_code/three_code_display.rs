//! Display implemented for the [threecode](ThreeCode) to allow the [threecode](ThreeCode)
//!  to be printed when the appropriate compiler flag is sent.

use std::{collections::HashMap, fmt::Display};

use lazy_static::__Deref;

use crate::intermediate::{DataRef, VarRepr};

use super::{BinOp, DataRefType, Function, OpSrc, Size, StatCode, StatNode, StatType, ThreeCode};

fn format_temp(var: &VarRepr) -> String {
    format!("V{}", var)
}

fn format_data_label(dref: &DataRef) -> String {
    format!("DRef[{}]", dref)
}

impl Display for OpSrc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpSrc::Const(i) => write!(f, "{}", i),
            OpSrc::DataRef(dref, offset) => {
                if offset == &0 {
                    write!(f, "{}", format_data_label(dref))
                } else {
                    write!(f, "({} + {})", format_data_label(dref), offset)
                }
            }
            OpSrc::Var(var) => write!(f, "{}", format_temp(var)),
            OpSrc::ReadRef => write!(f, "ReadRef"),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Mod => "%",
                BinOp::Eq => "==",
                BinOp::Ne => "!=",
                BinOp::Gt => ">",
                BinOp::Gte => ">=",
                BinOp::Lt => "<",
                BinOp::Lte => "<=",
                BinOp::And => "&&",
                BinOp::Or => "||",
                BinOp::Xor => "^",
            }
        )
    }
}
impl Display for Size {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Size::Byte => "byte",
                Size::Word => "word",
                Size::DWord => "dword",
            }
        )
    }
}

impl Display for StatCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StatCode::Assign(dst, src) => write!(f, "{} = {}", format_temp(dst), src),
            StatCode::AssignOp(dst, arg1, binop, arg2, checked) => write!(
                f,
                "{} = {} {} {} {}",
                format_temp(dst),
                arg1,
                binop,
                arg2,
                if *checked { "(Overflow Check)" } else { "" }
            ),
            StatCode::Load(dst, ptr, size) => {
                write!(f, "{} = ({}) *{}", format_temp(dst), size, ptr)
            }
            StatCode::Store(ptr, src, size) => {
                write!(f, "*{} = ({}) {}", ptr, size, format_temp(src))
            }
            StatCode::Call(dst, fun_name, args) => write!(
                f,
                "{} = {}({})",
                format_temp(dst),
                fun_name,
                args.iter().map(format_temp).collect::<Vec<_>>().join(",")
            ),
            StatCode::VoidCall(fun_name, args) => write!(
                f,
                "{}({})",
                fun_name,
                args.iter().map(format_temp).collect::<Vec<_>>().join(",")
            ),
        }
    }
}

fn get_next_node(label_map: &HashMap<StatNode, (usize, bool)>) -> Option<StatNode> {
    for (node, (_, translated)) in label_map {
        if !*translated {
            return Some(node.clone());
        }
    }
    None
}

fn display_routine(
    start_node: &StatNode,
    name: &str,
    args: &[VarRepr],
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    let mut label_map: HashMap<StatNode, (usize, bool)> = HashMap::new();

    let label_conv = |id: &usize| format!("b_{}_{}", name, id);

    let mut next = Some(start_node.clone());

    writeln!(
        f,
        "Def {}({}):",
        name,
        args.iter().map(format_temp).collect::<Vec<_>>().join(",")
    )?;

    while next.is_some() {
        while let Some(current) = next {
            let label_written = match label_map.get_mut(&current) {
                Some((id, true)) => {
                    writeln!(f, "\tgoto {}", label_conv(id))?;
                    break;
                }
                Some((id, l)) => {
                    writeln!(f, "{}:", label_conv(id))?;
                    *l = true;
                    true
                }
                None => {
                    label_map.insert(current.clone(), (label_map.len(), true));
                    false
                }
            };

            next = match current.get().deref() {
                StatType::Simple(prevs, stat, succ) => {
                    if let Some((id, _)) = label_map.get(&current) && prevs.len() > 1 {
                        writeln!(f, "{}:", label_conv(id))?;
                    }

                    writeln!(f, "\t{}", stat)?;
                    Some(succ.clone())
                }
                StatType::Branch(prevs, src, true_branch, succ) => {
                    if let Some((id, _)) = label_map.get(&current) && prevs.len() > 1 && !label_written {
                        writeln!(f, "{}:", label_conv(id))?;
                    }

                    let true_id = if let Some((id, _)) = label_map.get(true_branch) {
                        *id
                    } else {
                        let new_id = label_map.len();
                        label_map.insert(true_branch.clone(), (new_id, false));
                        new_id
                    };

                    writeln!(f, "\tif {} then goto {}", src, label_conv(&true_id))?;

                    Some(succ.clone())
                }
                StatType::Loop(prevs) => {
                    if let Some((id, _)) = label_map.get(&current) && prevs.len() > 1 && !label_written {
                        writeln!(f, "{}:", label_conv(id))?;
                    }

                    writeln!(f, "\tinfinitely loop")?;
                    None
                }
                StatType::Return(prevs, ret_val) => {
                    if let Some((id, _)) = label_map.get(&current) && prevs.len() > 1 && !label_written{
                        writeln!(f, "{}:", label_conv(id))?;
                    }

                    if let Some(ret_src) = ret_val {
                        writeln!(f, "\tReturn {}", ret_src)?;
                    } else {
                        writeln!(f, "\tReturn")?;
                    }

                    None
                }
                StatType::Dummy(_) => {
                    panic!("Dummy nodes should not be present in the displayed threecode")
                }
            };
        }

        next = get_next_node(&label_map);
    }

    writeln!(f)
}

fn print_string(string: &[u8], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "\tString: \"")?;
    for letter in String::from_utf8(string.to_owned())
        .expect("always valid utf8")
        .chars()
    {
        match letter {
            '\0' => write!(f, "\\0")?,
            '\n' => write!(f, "\\n")?,
            '\t' => write!(f, "\\t")?,
            '"' => write!(f, "\\\"")?,
            '\r' => write!(f, "\\r")?,
            '\u{0008}' => write!(f, "\\b")?,
            '\u{0012}' => write!(f, "\\f")?,
            c => write!(f, "{}", c)?,
        }
    }
    writeln!(f, "\"")
}

impl Display for DataRefType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataRefType::String(s) => print_string(s, f),
            DataRefType::Struct(members) => {
                let mut chars = vec![];

                for memb in members {
                    match memb {
                        (Size::Byte, t @ 0..=127) => chars.push(*t as u8),
                        (size, data) => {
                            if !chars.is_empty() {
                                print_string(&chars, f)?;
                                chars.clear()
                            }

                            writeln!(
                                f,
                                "\t{}: {}",
                                match size {
                                    Size::Byte => "byte",
                                    Size::Word => "word",
                                    Size::DWord => "dword",
                                },
                                data
                            )?;
                        }
                    }
                }
                if !chars.is_empty() {
                    print_string(&chars, f)?;
                }
                writeln!(f)
            }
        }
    }
}

impl Display for ThreeCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ThreeCode {
            functions,
            data_refs,
            graph: _,
            read_ref: _,
            code,
            int_handler,
        } = self;

        writeln!(f, "Data References:")?;
        for (dref, data) in data_refs {
            writeln!(f, "{}: \n{}", format_data_label(dref), data)?;
        }

        if let Some(int_handle) = int_handler {
            writeln!(f, "Integer Handler: {}", int_handle)?;
        } else {
            writeln!(f, "No Integer Handler")?;
        }

        display_routine(code, "main", &[], f)?;

        for (
            name,
            Function {
                args,
                code,
                graph: _,
                read_ref: _,
            },
        ) in functions
        {
            display_routine(code, name, args, f)?;
        }

        writeln!(f)
    }
}
