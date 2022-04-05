'a: {
    let f = fn<>(a : Int, b : Int) -> Int:
    'f: {
        if True || False then a else b
    };
    let mut x = 7;
    let res = f<>(4, x);
    x := 0;
    println("res: ", res);
    println("x: ", x);
    ()
}