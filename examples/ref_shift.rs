'a: {
    let mut x = 15;
    'b: {
        let mut p = &mut x;
        let mut y = ();
        'c: {
            let mut pp = &mut p;
            let mut y = ();
            let mut y = ();
            **pp := 123;
            let x_clone = clone **pp;
            println("x_clone: ", x_clone)
        }
    }
}