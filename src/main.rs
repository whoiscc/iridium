use iridium::{eval::Eval, parse::Parse, store::Store, Id};

fn main() {
    let prog = r#"
        procedure main() -> unit begin
            write_stdout("Hello, world!\n");
        end procedure
    "#;

    let mut machine = Eval::default();
    let mut store = Store::default();
    machine.insert_intrinsics(&mut store);

    for (type_attr, id) in Parse::new_cross_ref(prog, Id::root()) {
        store.insert_cross_ref(type_attr, id);
    }
    Parse::new(prog, Id::root(), &mut store).make();

    let main = store.get_main();
    machine.set_mem(store.into_mem());
    machine.eval_main(main);
}
