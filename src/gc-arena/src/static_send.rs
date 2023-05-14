use core::ops;

use crate::{Collect, Root, Rootable};

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
/// a Rootable projection of the held type which allows us to check that the projection of the
/// 'static lifetime is Send. Since the Rootable projection of 'static is itself 'static (from the
/// Rootable trait definition), we have evidence that if the held were known to be 'static, it would
/// also be known to be Send. We cannot get in trouble with mismatched projections because there's
/// only ever one way for a type with lifetime parameters to become 'static, all the lifetime
/// parameters must be 'static, we just need *proof* that when this type is 'static, it is Send.
///
/// This is extremely useful to get around the limitations of dyn types. By placing a Sized type
/// inside an StaticSend, we assert that it *would* be Send when placed into the arena root and
/// projected to 'static. We are then free to use the `unsize!` macro to convert this to a dyn type
/// without having to worry about Send bounds, and it will remain Send.
pub struct StaticSend<S: ?Sized> {
    inner: S,
}

unsafe impl<S: ?Sized + 'static> Send for StaticSend<S> {}

unsafe impl<S: ?Sized + Collect> Collect for StaticSend<S> {
    fn trace(&self, cc: crate::CollectionContext) {
        self.inner.trace(cc)
    }
}

impl<S: ?Sized> ops::Deref for StaticSend<S> {
    type Target = S;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<S: ?Sized> ops::DerefMut for StaticSend<S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub fn static_send<'gc, R: ?Sized + for<'a> Rootable<'a>>(
    r: Root<'gc, R>,
) -> StaticSend<Root<'gc, R>>
where
    Root<'static, R>: Send,
{
    StaticSend { inner: r }
}
