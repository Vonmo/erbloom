use serde::{Deserialize, Serialize};
use container::FilterType;

#[derive(Serialize, Deserialize, PartialEq, Clone, Copy)]
pub struct FilterOptions {
    pub filter_type: FilterType,
    pub bitmap_size: usize,
    pub items_count: usize,
    pub capacity: usize,
    pub rotate_at: usize,
    pub fp_rate: f64,
}

impl Default for FilterOptions {
    fn default() -> FilterOptions {
        FilterOptions {
            filter_type: FilterType::Bloom,
            bitmap_size: 10,
            items_count: 100,
            capacity: 3,
            rotate_at: 0,
            fp_rate: 0.1,
        }
    }
}