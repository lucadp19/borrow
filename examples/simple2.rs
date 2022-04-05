'a: {
    let x = 5;
    let p = &x;
    let pp = &p;
    let x1 = clone **pp;
    println(x1);
    ()
}