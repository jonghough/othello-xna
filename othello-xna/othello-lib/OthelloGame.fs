module OthelloGame

open System
open GameBoard
open SquareItem
open Minimax
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
 
type InputStateMachine<'InputState, 'Result> =
    | Active of ('InputState -> InputStateMachine<'InputState, 'Result>)
    | Done of 'Result

let loadImage device filename =
    Texture2D.FromStream(device, System.IO.File.OpenRead(filename))

let waitReleased pressed released func =
    let rec waitPressed() =
        fun (inputState) ->
            if pressed(inputState) then
                let result = func inputState
                waitReleased result
            else
                waitPressed()
        |> Active
    and waitReleased result =
        fun (inputState) ->
            if released(inputState) then
                Done result
            else
                waitReleased result
        |> Active
    waitPressed()


let UpdateInput (squareItems : seq<Square> byref) : bool =
    let ks = Keyboard.GetState()

    //if(ks.IsKeyDown(Keys.Space)) then
    //    System.Diagnostics.Debug.WriteLine("ok space down")

    let ms = Mouse.GetState()

    try
        if ms.LeftButton = ButtonState.Pressed then
            let clickedItem = Seq.filter (fun (sq : Square) ->  sq.isClicked(ms.X, ms.Y)) squareItems |> Seq.head
           // System.Diagnostics.Debug.WriteLine(" clicked on  "+ string(Seq.length clickedItems) + "squares")
            let s = CanPutPiece(squareItems, SquareState.WhiteCircle, clickedItem.col, clickedItem.row)
            System.Diagnostics.Debug.WriteLine("Can click item "+string(s))
            if s then 
                PutColor(&squareItems, clickedItem.col, clickedItem.row, SquareState.WhiteCircle)
                true
            else 
                false
        else 
            false
    with :? Exception ->
        false
        

let drawPosition (spritebatch : SpriteBatch) index bs ws =
    let row = index / 8
    let col = index % 8
    System.Diagnostics.Debug.WriteLine ( "row and column  " + string(row) + ", " + string(col))
    if (col % 2) + (row % 2) = 1 then
        spritebatch.Draw(ws, new Rectangle(100 + (50 * col),100 + (50 * row),50,50), Color.White)
    else
        spritebatch.Draw(bs, new Rectangle(100 + (50 * col),100 + (50 * row),50,50), Color.White)

type Gamex() as this =
    inherit Game()
    let mutable graphics = new GraphicsDeviceManager(this)
    let mutable bsquare = null 
    let mutable wsquare = null 
    let mutable bcircle = null
    let mutable wcircle = null
    let mutable spriteBatch = null
    let mutable texture = null
    let squareSeq = seq {0 .. 63}
    let mutable squareItemSeq = null 
    let mutable turn : SquareState = SquareState.WhiteCircle 
    let mutable timer = 0.0f
    let waitTime = 3.0f
    let mutable Font1 : SpriteFont = null 

    override Game.LoadContent() =
        graphics.IsFullScreen <- false
        this.IsMouseVisible <- true;
        graphics.PreferredBackBufferWidth <- 800
        graphics.PreferredBackBufferHeight <- 600
        graphics.ApplyChanges()
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        texture <- new Texture2D(this.GraphicsDevice, 1, 1)
        texture.SetData(Array.create 1 Color.White)
        bsquare <- loadImage this.GraphicsDevice @"C:\Users\jp20151\Documents\Visual Studio 2015\Projects\game-1\game-1\resources\blacksquare.png"
        wsquare <- loadImage this.GraphicsDevice @"C:\Users\jp20151\Documents\Visual Studio 2015\Projects\game-1\game-1\resources\whitesquare.png"
        bcircle <- loadImage this.GraphicsDevice @"C:\Users\jp20151\Documents\Visual Studio 2015\Projects\game-1\game-1\resources\blackcircle.png"
        wcircle <- loadImage this.GraphicsDevice @"C:\Users\jp20151\Documents\Visual Studio 2015\Projects\game-1\game-1\resources\whitecircle.png"
        
        this.Content.RootDirectory <- "Content"
        Font1 <- this.Content.Load<SpriteFont>("SpriteFont1")
        squareItemSeq <- seq { for index in 0 .. 63 do
                                    let row = index / 8
                                    let col = index % 8
                                    if (col % 2) + (row % 2) = 1 then
                                        yield new SquareItem.Square(wsquare, wcircle, bcircle, 100 + (50 * col), 100 + (50 * row),col, row)
                                    else
                                        yield new SquareItem.Square(bsquare, wcircle, bcircle, 100 + (50 * col), 100 + (50 * row), col, row )
                            }

       

    member this.Check(squareItems : seq<Square> byref, col, row) =
                    if CanPutPiece(squareItems, SquareState.BlackCircle, col, row) then
                        PutColor(&squareItems, col, row, SquareState.BlackCircle)
                        true
                    else
                        false

    member this.CalculateWinner() =
        let whiteCount = Seq.filter (fun (i : Square) -> i.GetSquareState() = SquareState.WhiteCircle) squareItemSeq |> Seq.length
        let blackCount = Seq.filter (fun (i : Square) -> i.GetSquareState() = SquareState.BlackCircle) squareItemSeq |> Seq.length
        if whiteCount > blackCount then
            SquareState.WhiteCircle
        else if whiteCount < blackCount then
            SquareState.BlackCircle
        else
            SquareItem.NoCircle

    override Game.Update gameTime =
        base.Update gameTime

        match turn with
        | SquareState.WhiteCircle ->
            if GameBoard.GetPossibleMoves(squareItemSeq, SquareState.WhiteCircle) |> Seq.length = 0 then
                if GameBoard.GetPossibleMoves(squareItemSeq, SquareState.BlackCircle) |> Seq.length = 0 then
                    turn <- SquareState.NoCircle
                    match this.CalculateWinner() with
                    | SquareState.WhiteCircle -> System.Diagnostics.Debug.WriteLine("WHITE WINNERS")
                    | SquareState.BlackCircle -> System.Diagnostics.Debug.WriteLine("BLACK WINNERS")
                    | _ -> System.Diagnostics.Debug.WriteLine("NO WINNER")
                else
                    turn <- SquareState.BlackCircle
            else
                let finishTurn = UpdateInput(&squareItemSeq)
                if finishTurn then
                    turn <- SquareState.BlackCircle
        | SquareState.BlackCircle ->
            timer <- timer + float32( gameTime.ElapsedGameTime.Milliseconds) / 1000.0f
            if timer > 0.6f then
                timer <- 0.0f

                let geval (board : Square seq, color : SquareState) =
                    seq { for item in board do
                                   if item.GetSquareState() = color then
                                        let scre = match (item.col, item.row) with
                                                   | (0,0) | (0, 7) | (7, 0) | (7,7) -> 5
                                                   | (0,_) | (7, _) | (_, 0) | (_,7) -> 2
                                                   | _ -> 1
                                        yield scre
                                   else if item.GetSquareState() = GameBoard.GetEnemyColor color then
                                        let scre = match (item.col, item.row) with
                                                   | (0,0) | (0, 7) | (7, 0) | (7,7) -> -5
                                                   | (0,_) | (7, _) | (_, 0) | (_,7) -> -2
                                                   | _ -> -1
                                        yield scre
                                   else yield 0
                                   } |> Seq.reduce (+)

                let f board =
                    Seq.filter ( fun (a : Square) -> a.GetSquareState() = SquareState.BlackCircle) board |> Seq.length

                let mm = new MinimaxController(2, fun (brd : Square seq, colr : SquareState) -> geval(brd, colr))

                let score = mm.Evaluate(squareItemSeq, SquareState.BlackCircle)
                match score with
                | None -> ()
                | Some sc -> PutColor(&squareItemSeq, sc.Position.col, sc.Position.row, SquareState.BlackCircle)

                turn <- SquareState.WhiteCircle
        | _ -> ()


    override Game.Draw gameTime =
        this.GraphicsDevice.Clear(Color.CornflowerBlue)
        spriteBatch.Begin()
        Seq.iter (fun (sq : Square) -> sq.draw spriteBatch) squareItemSeq |> ignore
        spriteBatch.DrawString(Font1, "Othello",  new Vector2(10.0f, 10.0f), Color.White);
        //SpriteFont()
        spriteBatch.End()
        base.Draw gameTime
