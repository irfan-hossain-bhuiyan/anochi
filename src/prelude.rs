use std::mem::size_of;

fn slice_index_from_vec<T>(vec: &[T], slice: &[T]) -> Option<usize> {
    let vec_start = vec.as_ptr() as usize;
    let slice_start = slice.as_ptr() as usize;

    // Check if slice is within vec
    if slice_start >= vec_start && slice_start <= vec_start + std::mem::size_of_val(vec) {
        let offset_bytes = slice_start - vec_start;
        Some(offset_bytes / size_of::<T>())
    } else {
        None
    }
}
