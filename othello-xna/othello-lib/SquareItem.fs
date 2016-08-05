module SquareItem

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type SquareState = | NoCircle | WhiteCircle | BlackCircle

type Square (sqTexture, wCircleTexture, bCircleTexture, positionx, positiony, col, row) as this =
    let mutable squareState = SquareState.WhiteCircle
    do squareState <- this.init(col, row)

    member this.squareTexture = sqTexture
    member this.whiteCircleTexture = wCircleTexture
    member this.blackCircleTexture = bCircleTexture
    member this.positionx = positionx
    member this.positiony = positiony
    member this.rect = new Rectangle(this.positionx, this.positiony, 50,50);
    member this.col = col
    member this.row = row
    
    

    member this.init(x,y) : SquareState =
        match x,y with
        | 3,3 ->  WhiteCircle
        | 3,4 ->  BlackCircle
        | 4,3 ->  BlackCircle
        | 4,4 ->  WhiteCircle
        | _,_ ->  NoCircle

    member public this.GetSquareState () =
        squareState

    member public this.SetSquareState (nextState : SquareState) =
        squareState <- nextState

    member public this.draw(spritebatch : SpriteBatch) : Unit =
    
        spritebatch.Draw(this.squareTexture, this.rect, Color.White);

        match squareState with
        | SquareState.WhiteCircle -> spritebatch.Draw(this.whiteCircleTexture, this.rect, Color.White)
        | SquareState.BlackCircle -> spritebatch.Draw(this.blackCircleTexture, this.rect, Color.White)
        | _ -> ()

    member public this.isClicked (x, y) : bool =
        this.positionx < x && this.positionx + 50 >= x && this.positiony < y && this.positiony + 50 >= y
            
            