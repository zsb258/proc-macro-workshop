use seq::seq;

// macro_rules! expand_to_nothing {
//     ($arg:literal) => {
//         // nothing
//     };
// }
//
// seq!(N in 0..4 {
//     expand_to_nothing!(N);
// });

// seq!(N in 1..4 {
//     fn f~N () -> u64 {
//         N * 2
//     }
// });

seq!(N in 0..3 {
    // #[derive(Copy, Clone, PartialEq, Debug)]
    enum Interrupt {
        #(
            Irq~N,
        )*
    }
});

fn main() {}
