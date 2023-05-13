use compy::python::{parse, serialize, serialize_inlined};
use compy::transform::oneline::*;
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn transform_nothing(source: String) -> String {
    serialize(parse(&source.as_str()))
}

#[wasm_bindgen]
pub fn transform_oneline(source: String) -> String {
    serialize_inlined(oneline(parse(&source.as_str())))
}
