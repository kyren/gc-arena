use core::alloc::Layout;

#[cfg(alloc_layout_extra)]
#[inline]
pub(crate) fn layout_padding_needed_for(this: &Layout, align: usize) -> usize {
    layout.padding_needed_for(align)
}

#[cfg(not(alloc_layout_extra))]
#[inline]
pub(crate) fn layout_padding_needed_for(this: &Layout, align: usize) -> usize {
    let len = this.size();
    let len_rounded_up = len.wrapping_add(align).wrapping_sub(1) & !align.wrapping_sub(1);
    len_rounded_up.wrapping_sub(len)
}
