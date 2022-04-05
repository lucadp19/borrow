'a: {
    let f = fn<'a>(x : &'a Int, y: &'a Int) -> Int:
    'f: {
        let x1 = (clone *x) + (clone *y);
        x1
    };
    let x = 5;
    let y = 6;
    let res = f<'a>(&x, &y);
    println("res: ", res);
    ()
}