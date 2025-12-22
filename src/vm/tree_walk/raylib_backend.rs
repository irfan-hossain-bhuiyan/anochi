use crate::vm::backend::{ BackendResult, VmBackend};
use crate::vm::tree_walk::scope_stack::ScopeStack;
use crate::types::{TypeContainer};
use crate::vm::tree_walk::{ VmValueGeneralized, VmValueSimplified, ParsedValueType};
use crate::vm::tree_walk::vm_error::VmErrorType;
use raylib::prelude::*;
//use num_traits::cast::ToPrimitive;


pub struct RayLibBackend {
    rl: Option<RaylibHandle>,
    thread: Option<RaylibThread>,
}

impl Default for RayLibBackend {
    fn default() -> Self {
        Self {
            rl: None,
            thread: None,
        }
    }
}

impl RayLibBackend {
    pub fn new() -> Self {
        Self::default()
    }
}

impl std::fmt::Debug for RayLibBackend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RayLibBackend")
    }
}

impl VmBackend for RayLibBackend {
    fn debug_print(&mut self, message: &str) -> BackendResult<()> {
        if let (Some(rl), Some(thread)) = (&mut self.rl, &mut self.thread) {
            let mut d = rl.begin_drawing(thread);
            d.clear_background(Color::WHITE);
            d.draw_text(message, 10, 10, 20, Color::BLACK);
            BackendResult::Ok(())
        } else {
            // Fallback if not initialized? Or error?
            // User said "on debug it prints on the raylib window"
            // If window not open, maybe just print to stdout
            println!("[RayLib Disabled]: {}", message);
            Ok(())
        }
    }

    fn read_input(&mut self, _prompt: Option<&str>) -> BackendResult<Option<String>> {
        Ok(None) // Raylib handled input differently usually
    }

    fn print(&mut self, message: &str) -> BackendResult<()> {
        println!("{}", message);
        Ok(())
    }

    fn print_error(&mut self, message: &str) -> BackendResult<()> {
        eprintln!("{}", message);
        Ok(())
    }

    fn flush(&mut self) -> BackendResult<()> {
        Ok(())
    }

    fn has_input(&self) -> bool {
        false
    }

    fn has_output(&self) -> bool {
        true
    }

    fn initialize(&mut self) -> BackendResult<()> {
        let (rl, thread) = raylib::init()
            .size(640, 480)
            .title("Anochi Raylib Backend")
            .build();
        self.rl = Some(rl);
        self.thread = Some(thread);
        
        // Disable exit key for now to handle it manually if needed, or default
        // self.rl.as_mut().unwrap().set_exit_key(None);
        
        Ok(())
    }

    fn call_foreign(
        &mut self,
        name: &str,
        _scope: &mut ScopeStack,
        types: &mut TypeContainer,
    ) -> Result<VmValueGeneralized, VmErrorType> {
        match name {
            "window_should_close" => {
                if let Some(rl) = &self.rl {
                     let res = rl.window_should_close();
                     Ok(VmValueSimplified::ValuePrimitive(crate::vm::tree_walk::ValuePrimitive::Bool(res)).into_vm_value_generalized(types))
                } else {
                    Err(VmErrorType::ForeignError("Raylib not initialized".to_string()))
                }
            }
             "begin_drawing" => {
                 Ok(VmValueSimplified::create_unit().into_vm_value_generalized(types))
             }
             "end_drawing" => {
                  Ok(VmValueSimplified::create_unit().into_vm_value_generalized(types))
             }
             "draw_fps" => {
                 if let (Some(rl), Some(thread)) = (&mut self.rl, &self.thread) {
                      let mut d = rl.begin_drawing(thread);
                      d.draw_fps(10, 10);
                 }
                 Ok(VmValueSimplified::create_unit().into_vm_value_generalized(types))
             }
             "draw_circle" => {
                      let x = Self::get_int_arg("x", _scope, types)? as i32;
                      let y = Self::get_int_arg("y", _scope, types)? as i32;
                      let radius = Self::get_float_arg("radius", _scope, types)? as f32;
                 if let (Some(rl), Some(thread)) = (&mut self.rl, &self.thread) {
                      let mut d = rl.begin_drawing(thread);
                      d.draw_circle(x, y, radius, Color::RED);
                 }
                 Ok(VmValueSimplified::create_unit().into_vm_value_generalized(types))
             }
             "draw_rectangle" => {
                      let x = Self::get_int_arg("x", _scope, types)? as i32;
                      let y = Self::get_int_arg("y", _scope, types)? as i32;
                      let width = Self::get_int_arg("width", _scope, types)? as i32;
                      let height = Self::get_int_arg("height", _scope, types)? as i32;
                 if let (Some(rl), Some(thread)) = (&mut self.rl, &self.thread) {
                      let mut d = rl.begin_drawing(thread);
                      d.draw_rectangle(x, y, width, height, Color::BLUE);
                 }
                 Ok(VmValueSimplified::create_unit().into_vm_value_generalized(types))
             }
             "clear_background" => {
                 if let (Some(rl), Some(thread)) = (&mut self.rl, &self.thread) {
                      let mut d = rl.begin_drawing(thread);
                      d.clear_background(Color::RAYWHITE);
                 }
                 Ok(VmValueSimplified::create_unit().into_vm_value_generalized(types))
             }
            _ => Err(VmErrorType::ForeignError(format!("Unknown raylib function: {}", name)))
        }
    }

    fn get_foreign_signatures(&self) -> Vec<crate::vm::backend::ForeignFuncSignature> {
        use crate::vm::backend::ForeignFuncSignature;
        vec![
            ForeignFuncSignature {
                name: "window_should_close".to_string(),
                params: vec![],
                return_type: Some("bool".to_string()),
            },
            ForeignFuncSignature {
                name: "begin_drawing".to_string(),
                params: vec![],
                return_type: None,
            },
            ForeignFuncSignature {
                name: "end_drawing".to_string(),
                params: vec![],
                return_type: None,
            },
            ForeignFuncSignature {
                name: "draw_fps".to_string(),
                params: vec![],
                return_type: None,
            },
            ForeignFuncSignature {
                name: "draw_circle".to_string(),
                params: vec![("x".to_string(), "int".to_string()), ("y".to_string(), "int".to_string()), ("radius".to_string(), "float".to_string())],
                return_type: None,
            },
            ForeignFuncSignature {
                name: "draw_rectangle".to_string(),
                params: vec![("x".to_string(), "int".to_string()), ("y".to_string(), "int".to_string()), ("width".to_string(), "int".to_string()), ("height".to_string(), "int".to_string())],
                return_type: None,
            },
            ForeignFuncSignature {
                name: "clear_background".to_string(),
                params: vec![],
                return_type: None,
            },
        ]
    }
}

impl RayLibBackend {
    fn get_int_arg(name: &str, scope: &ScopeStack, types: &TypeContainer) -> Result<i64, VmErrorType> {
         let val = scope.get_value_from_name(&Identifier::new(name.to_string()), types)?;
         let simplified = val.into_simplified_value(types);
         match simplified {
             VmValueSimplified::ValuePrimitive(crate::vm::tree_walk::ValuePrimitive::Integer(i)) => {
                 Ok(i.try_into().unwrap_or(0))
             }
             _ => Err(VmErrorType::TypeMismatch("Expected int argument")),
         }
    }
    
    fn get_float_arg(name: &str, scope: &ScopeStack, types: &TypeContainer) -> Result<f64, VmErrorType> {
         let val = scope.get_value_from_name(&crate::ast::Identifier::new(name.to_string()), types)?;
         let simplified = val.into_simplified_value(types);
         // Handle float (BigRational) to f64 conversion if possible, or assume explicit Float primitive
         match simplified {
             VmValueSimplified::ValuePrimitive(crate::vm::tree_walk::ValuePrimitive::Float(f)) => {
                 Ok(f.to_f64().unwrap_or(0.0))
             }
             _ => Err(VmErrorType::TypeMismatch("Expected float argument")),
         }
    }
}
