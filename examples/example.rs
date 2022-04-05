'a: {
    let mut x = "string";
    let y = 'b: {
        let p = &mut x;
        let func = fn<'l>(ptr : &'l mut String) -> ():
        'f: {
            *ptr := "hello"
        };
        func<'b>(p);
        " world"
    };
    println(x ++ y)
}