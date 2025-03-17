open Printf

let rec last (xs : 'a list) : 'a option =
    match xs with
    | []        -> None
    | [x]       -> Some x
    | _ :: tail -> last tail

let rec last_two (xs : 'a list) : ('a * 'a) option =
    match xs with
    | [] | [_]  -> None
    | [a ; b]   -> Some (a, b)
    | _ :: tail -> last_two tail

let rec at (idx : int) (xs : 'a list) : 'a option =
    match xs with
    | [] -> None
    | head :: tail ->
        if idx = 0 then
            Some head
        else
            at (idx-1) tail

let rec length (xs : 'a list) : int =
    let rec aux i = function
        | [] -> i
        | _ :: tail -> aux (i+1) tail
    in aux 0 xs

let rec rev (xs : 'a list) : 'a list =
    let rec aux r = function
        | [] -> r
        | head :: tail -> aux (head :: r) tail
    in aux [] xs

let is_palindrome (xs : 'a list) : bool =
    rev xs = xs

let () =
    assert ((last [1 ; 2 ; 3]) = (Some 3));
    assert ((last []) = None);
    assert ((last_two [1 ; 2 ; 3]) = (Some (2, 3)));
    assert ((last_two [1]) = None);
    assert ((last_two []) = None);
    assert ((at 0 []) = None);
    assert ((at 1 [1 ; 2 ; 3]) = Some 2);
    assert ((at 3 [1 ; 2 ; 3]) = None);
    assert ((at 2 [1 ; 2 ; 3]) = Some 3);
    assert ((length []) = 0);
    assert ((length [1 ; 2 ; 3]) = 3);
    assert ((length [1 ; 2 ; 3 ; 4 ; 5 ; 6]) = 6);
    assert ((rev [1 ; 2 ; 3]) = [3 ; 2 ; 1]);
    assert ((rev [1]) = [1]);
    assert ((rev []) = []);
    assert ((is_palindrome [1 ; 2 ; 3]) = false);
    assert ((is_palindrome [1 ; 2 ; 1]) = true);
