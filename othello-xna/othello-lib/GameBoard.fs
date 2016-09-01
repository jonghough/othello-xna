module GameBoard
open SquareItem

///
///
///
let rec CanFindColorOnLine(squareSeq : seq<Square>, xpoint : int, ypoint : int, xdir : int, ydir : int, maxSize : int, squareColor : SquareState) =
        if xdir = 0 && ydir = 0 then
            false
        else
            match  Seq.tryFind (fun (x : Square) -> x.col = xpoint + xdir && x.row = ypoint + ydir) squareSeq with
            | None -> false
            | Some sq ->
                if sq.GetSquareState() = squareColor then
                    true
                else if sq.GetSquareState() = SquareState.NoCircle then
                    false
                else 
                    CanFindColorOnLine(squareSeq, xpoint + xdir, ypoint + ydir, xdir, ydir, maxSize, squareColor)

///
///
///        
let GetEnemyColor color =
                match color with 
                | SquareState.WhiteCircle -> SquareState.BlackCircle
                | _ -> SquareState.WhiteCircle

///<summary>
/// Returns true if the color can be put in the (i,j) place.
///</summary>
let CanPutPiece(squareSeq : seq<Square>, squareColor : SquareState, i : int, j : int) =
    match  Seq.tryFind (fun (x : Square) -> x.col = i && x.row = j) squareSeq with 
    | None -> false
    | Some sq ->
        if sq.GetSquareState() <> SquareState.NoCircle then
            false
        else
            let enemy = 
                match squareColor with
                | SquareState.BlackCircle -> SquareState.WhiteCircle
                | _ -> SquareState.BlackCircle

            let possible = [for x in -1 .. 1 do for y in -1 .. 1 -> 
                                                                    match (Seq.tryFind (fun (sq : Square) -> sq.col = i + x && sq.row = j + y)  squareSeq) with
                                                                    | None -> false
                                                                    | Some square ->
                                                                        if square.GetSquareState() = enemy then
                                                                            CanFindColorOnLine(squareSeq, i, j, x, y, 8, squareColor)
                                                                        else
                                                                            false]
            List.filter (fun l -> l) possible |> List.length > 0


///
///
///
let PutColor(squareSeq : seq<Square> byref, i : int, j : int, squareColor : SquareState) =
    
    squareSeq <- Seq.map (fun (r : Square) -> if r.col = i && r.row = j then 
                                                r.SetSquareState(squareColor)
                                                r
                                              else r) squareSeq

    let enemy = 
        match squareColor with
        | SquareState.BlackCircle -> SquareState.WhiteCircle
        | _ -> SquareState.BlackCircle

    let mutable nextSquareSeq = squareSeq

    let rec g(a : int, b : int, xdir : int, ydir : int) =
        if xdir = 0 && ydir = 0 then
            ()
        else
            match (Seq.tryFind (fun (sq : Square) -> sq.col = a && sq.row = b)  nextSquareSeq) with
            | None -> ()
            | Some square ->
                if square.GetSquareState() = enemy then
                    nextSquareSeq <- Seq.map (fun (r : Square) -> if r.col = a && r.row = b then 
                                                                    r.SetSquareState(squareColor)
                                                                    r
                                                                  else r) nextSquareSeq
                    g(a - xdir, b - ydir, xdir, ydir)
                else ()
                                                                


    let rec f(a : int, b : int, xdir : int, ydir : int) =
        if xdir = 0 && ydir = 0 then
            ()
        else 
            match (Seq.tryFind (fun (sq : Square) -> sq.col = a && sq.row = b)  nextSquareSeq) with
            | None -> ()
            | Some square ->
                if square.GetSquareState() = enemy then
                    f(a + xdir, b + ydir, xdir, ydir)
                else if square.GetSquareState() = squareColor then
                    g(a - xdir, b - ydir, xdir, ydir)
                else ()

    [for x in -1 .. 1 do for y in -1 .. 1 -> f(i + x, j + y, x, y)] |> ignore
    squareSeq <- nextSquareSeq


///
///
///
let GetPossibleMoves (squareSeq : seq<Square>, squareColor : SquareState) =

    let emptySquares = Seq.filter (fun (sq : Square) -> sq.GetSquareState() = SquareState.NoCircle) squareSeq
    if Seq.length emptySquares = 0 then
        Seq.empty
    else
        let possible = Seq.map (fun (sq : Square) -> 
                                    if CanPutPiece(squareSeq, squareColor, sq.col, sq.row) then
                                        Some sq
                                    else
                                        None) emptySquares
        seq { for item in possible do
                                match item with
                                | Some i -> yield i
                                | _ -> () }