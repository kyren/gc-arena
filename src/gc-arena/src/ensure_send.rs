use core::{marker::PhantomData, ops};

use crate::{types::Invariant, Collect, MutationContext, Root, Rootable};

/// Assert that a contained type implements `Send` when we can observe that 'gc is actually 'static.
///
/// Mutation callbacks in `Arena` must work for any possible lifetime 'gc. In reality, the provided
/// 'gc is really always 'static, but the mutation callbacks cannot observe this fact. The fact tha
/// they must work for any possible 'gc is crucial to the soundness of the gc-arena crate, as it is
/// what prevents Gc objects from leaking or being confused between arenas.
///
/// However, in reality, from the outside of an Arena we can observe that the branding 'gc lifetime
/// is *actually* 'static, which is why Gc can safely implement Send when 'gc is 'static and Arena
/// can be Send if its root object is Send. This means that as long as the root type only stores
/// normally Send types and gc-arena pointers, it will be Send when "at rest" viewwed from outside
/// the Arena, even though this is not the case when viewed from the inside via a callback. This is
/// extremely useful, as it allows a sort of "Send jail", where when interacting with Gc pointers,
/// they are !Send, but we can still send an entire arena to another thread if there are no
/// currently running mutation callbacks.
///
/// This solution is simple and works, but there is one wrinkle. Dyn types must forget their actual
/// held type, and there is no way to say that "once this dyn type becomes 'static, it is then
/// Send". Instead, it will be impossible to get a dyn Send object because the 'gc lifetime cannot
/// be known to be 'static, so the object cannot be known to be Send *at that time*.
///
/// This struct is a simple wrapper that gets around this problem. On construction, we must provide
/// a Rootable projection of the held type along with a `MutationContext<'gc>`, and this allows us
/// to assert all of the following:
///   1) Our held type, when projected to 'static, must be Send
///   2) We currently have a MutationContext as evidence we are inside an arena callback, and to
///      brand the constructed type with this unique lifetime branding.
///   3) Our held type is projected with the same provided Rootable to the 'gc of the
///      MutationContext, ensuring that everywhere we required 'static to be Send, we now have a
///      known 'gc branding lifetime (which is actually 'static).
///
/// Knowing these facts allows us to conditionally implement Send when we can observe that the
/// branding lifetime is actually 'static, while *ignoring the inner type*. Since we have proven
/// that the proper conditions hold at the time of creation, we don't need to check the inner type
/// for Send.
///
/// This is extremely useful to get around the limitations of dyn types. By placing a Sized type
/// inside an EnsureSend, we assert that it *would* be Send when placed into the arena root and
/// projected to 'static. We are then free to use the `unsize!` macro to convert this to a dyn type
/// without having to worry about Send bounds, and it will remain Send.
pub struct EnsureSend<'gc, S: ?Sized + 'gc> {
    _invariant: Invariant<'gc>,
    inner: S,
}

unsafe impl<S: ?Sized> Send for EnsureSend<'static, S> {}

unsafe impl<'gc, S: Collect> Collect for EnsureSend<'gc, S> {
    fn trace(&self, cc: crate::CollectionContext) {
        self.inner.trace(cc)
    }
}

impl<'gc, S: ?Sized> ops::Deref for EnsureSend<'gc, S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'gc, S: ?Sized> ops::DerefMut for EnsureSend<'gc, S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub fn ensure_send<'gc, R: ?Sized + for<'a> Rootable<'a>>(
    _mc: MutationContext<'gc, '_>,
    r: Root<'gc, R>,
) -> EnsureSend<'gc, Root<'gc, R>>
where
    Root<'static, R>: Send,
{
    EnsureSend {
        _invariant: PhantomData,
        inner: r,
    }
}
