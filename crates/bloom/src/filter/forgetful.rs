use super::FilterType;
use bloomfilter::Bloom;
use container::{RawSerializedFilter, SerializedFilter};
use options::FilterOptions;

type Result<T> = std::result::Result<T, String>;

pub struct ForgetfulFilter {
    pub filters: Vec<Bloom<[u8]>>,
    pub items_count: usize,
    pub capacity: usize,
    pub rotate_at: usize,
    pub insertion_count: usize,
}

impl ForgetfulFilter {
    pub fn new(opts: FilterOptions) -> Result<ForgetfulFilter> {
        assert_eq!(opts.filter_type, Some(FilterType::Forgetful));

        let capacity = opts.capacity.ok_or("capacity not set")?;
        let rotate_at = opts.rotate_at.ok_or("rotate_at  not set")?;
        let items_count = opts.items_count.ok_or("items_count not set")?;

        let mut filters = Vec::with_capacity(capacity);
        for _ in 0..capacity {
            let filter = match opts {
                FilterOptions {
                    bitmap_size: Some(bitmap_size),
                    items_count: Some(items_count),
                    fp_rate: None,
                    ..
                } => Bloom::new(bitmap_size, items_count),
                FilterOptions {
                    bitmap_size: None,
                    items_count: Some(items_count),
                    fp_rate: Some(fp_rate),
                    ..
                } => Bloom::new_for_fp_rate(items_count, fp_rate),
                _ => {
                    return Err(format!(
                        "must set `items_count` AND (`fp_rate` OR `bitmap_size`)], got {:?}",
                        opts
                    ))
                }
            };
            filters.push(filter)
        }

        Ok(ForgetfulFilter {
            filters,
            items_count,
            capacity,
            rotate_at,
            insertion_count: 0,
        })
    }

    pub fn set(&mut self, key: &[u8]) -> bool {
        let num_inner_filters = self.filters.len();

        // check membership
        let mut member = false;
        // check the overlapping blooms 2 by 2
        for x in 0..num_inner_filters - 2 {
            if self.filters[x].check(key) && self.filters[x + 1].check(key) {
                member = true;
                break;
            }
        }

        if !member {
            // check last bloom
            member = self.filters[num_inner_filters - 1].check(key);
        }
        if !member {
            self.insertion_count += 1;
            if self.insertion_count >= self.rotate_at {
                self.insertion_count = 0;
                // rotate the oldest bloom to the start of the list
                // and clear it
                self.filters.rotate_right(1);
                self.filters[0].clear();
            }
            // set in the future and current
            self.filters[0].set(key);
            self.filters[1].set(key);
        }

        member
    }

    pub fn check(&self, key: &[u8]) -> bool {
        let num_inner_filters = self.filters.len();
        // check the overlapping blooms 2 by 2
        for x in 0..num_inner_filters - 2 {
            if self.filters[x].check(&key) && self.filters[x + 1].check(&key) {
                return true;
            }
        }
        // check last bloom
        self.filters[num_inner_filters - 1].check(&key)
    }

    pub fn clear(&mut self) {
        let num_inner_filters = self.filters.len();
        for x in 0..num_inner_filters - 2 {
            self.filters[x].clear()
        }
    }

    pub fn serialize(&self) -> Result<Vec<u8>> {
        let mut opts = FilterOptions::default();
        opts.filter_type = Some(FilterType::Forgetful);
        opts.items_count = Some(self.items_count);
        opts.capacity = Some(self.capacity);
        opts.rotate_at = Some(self.rotate_at);

        let mut serialized_filters: Vec<RawSerializedFilter> = vec![];
        for i in 0..self.capacity {
            let filter = &self.filters[i];
            let sips = filter.sip_keys();
            let bitmap = filter.bitmap();
            serialized_filters.push(RawSerializedFilter {
                payload: bitmap,
                num_bits: filter.number_of_bits(),
                num_funs: filter.number_of_hash_functions(),
                sip00: sips[0].0,
                sip01: sips[0].1,
                sip10: sips[1].0,
                sip11: sips[1].1,
            })
        }
        bincode::serialize(&SerializedFilter {
            filters: serialized_filters,
            opts,
            upsert_num: self.insertion_count as u64,
        })
        .map_err(|e| format!("bincode serialization failed with: {}", e))
    }

    pub fn restore(prev_filter: SerializedFilter) -> ForgetfulFilter {
        let mut filters: Vec<Bloom<[u8]>> = vec![];

        for i in 0..prev_filter.opts.capacity.unwrap() {
            let pf = &prev_filter.filters.to_vec()[i];
            filters.push(Bloom::from_existing(
                &pf.payload,
                pf.num_bits,
                pf.num_funs,
                [(pf.sip00, pf.sip01), (pf.sip10, pf.sip11)],
            ))
        }

        ForgetfulFilter {
            filters,
            items_count: prev_filter.opts.items_count.unwrap(),
            capacity: prev_filter.opts.capacity.unwrap(),
            rotate_at: prev_filter.opts.rotate_at.unwrap(),
            insertion_count: prev_filter.upsert_num as usize,
        }
    }
}
