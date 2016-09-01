module OthelloGame

open System
open GameBoard
open SquareItem
open Minimax
open XnaButton
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System.IO

type GameState = Home | Game | Result



let UpdateInput (squareItems : seq<Square> byref) : bool =
   
    let ms = Mouse.GetState()

    try
        if ms.LeftButton = ButtonState.Pressed then
            let clickedItem = Seq.filter (fun (sq : Square) ->  sq.isClicked(ms.X, ms.Y)) squareItems |> Seq.head
            let s = CanPutPiece(squareItems, SquareState.WhiteCircle, clickedItem.col, clickedItem.row)
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
    if (col % 2) + (row % 2) = 1 then
        spritebatch.Draw(ws, new Rectangle(100 + (50 * col),100 + (50 * row),50,50), Color.White)
    else
        spritebatch.Draw(bs, new Rectangle(100 + (50 * col),100 + (50 * row),50,50), Color.White)

type Gamex() as this =
    inherit Microsoft.Xna.Framework.Game()
    let mutable graphics = new GraphicsDeviceManager(this)
    let mutable gsquare = null 
    let mutable bcircle = null
    let mutable wcircle = null
    let mutable spriteBatch = null
    let mutable texture = null
    let mutable starte = null
    let mutable startm = null
    let mutable starth = null
    let mutable start2 = null
    let mutable start3 = null
    let mutable reset = null

    let squareSeq = seq {0 .. 63}
    let mutable squareItemSeq = null 
    let mutable turn : SquareState = SquareState.WhiteCircle 
    let mutable timer = 0.0f
    let waitTime = 3.0f
    let mutable Font1 : SpriteFont = null 
    let mutable state : GameState = GameState.Home
    let mutable depth = 1
    let mutable startBtnE = new Button(100, 100, 300, 50, starte, start2, start3, fun () -> (state <- Game))
    let mutable startBtnM = new Button(100, 100, 300, 50, startm, start2, start3, fun () -> (state <- Game))
    let mutable startBtnH = new Button(100, 100, 300, 50, starth, start2, start3, fun () -> (state <- Game))
    let mutable resetBtn = new Button(100, 100, 300, 50, starth, start2, start3, fun () -> (state <- Home))

    override Game.LoadContent() =
        graphics.IsFullScreen <- false
        this.IsMouseVisible <- true;
        graphics.PreferredBackBufferWidth <- 800
        graphics.PreferredBackBufferHeight <- 600
        graphics.ApplyChanges()
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        texture <- new Texture2D(this.GraphicsDevice, 1, 1)
        texture.SetData(Array.create 1 Color.White)
        
        this.Content.RootDirectory <- "Content"
        gsquare <- this.Content.Load<Texture2D>("greensquare")
        bcircle <- this.Content.Load<Texture2D>("blackcircle")
        wcircle <- this.Content.Load<Texture2D>("whitecircle")
        starte <- this.Content.Load<Texture2D>("starteasybutton")
        startm <- this.Content.Load<Texture2D>("startmediumbutton")
        starth <- this.Content.Load<Texture2D>("starthardbutton")
        start2 <- this.Content.Load<Texture2D>("startbutton2")
        start3 <- this.Content.Load<Texture2D>("startbutton3")
        reset <- this.Content.Load<Texture2D>("resetbutton")

        startBtnE <- new Button(100, 100, 300, 50, starte, start2, start3, fun () -> (state <- GameState.Game; depth <- 1))
        startBtnM <- new Button(100, 200, 300, 50, startm, start2, start3, fun () -> (state <- GameState.Game; depth <- 2))
        startBtnH <- new Button(100, 300, 300, 50, starth, start2, start3, fun () -> (state <- GameState.Game; depth <- 5))
        resetBtn <- new Button(100, 520, 300, 50, reset, start2, start3, fun () -> (state <- GameState.Home))
      
        Font1 <- this.Content.Load<SpriteFont>("SpriteFont1")
        squareItemSeq <- seq { for index in 0 .. 63 do
                                    let row = index / 8
                                    let col = index % 8
                                    if (col % 2) + (row % 2) = 1 then
                                        yield new SquareItem.Square(gsquare, wcircle, bcircle, 100 + (50 * col), 100 + (50 * row),col, row)
                                    else
                                        yield new SquareItem.Square(gsquare, wcircle, bcircle, 100 + (50 * col), 100 + (50 * row), col, row )
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
        match state with
        | Home -> 
            let dt = float32( gameTime.ElapsedGameTime.Milliseconds) / 1000.0f
            startBtnE.Update(dt)
            startBtnM.Update(dt)
            startBtnH.Update(dt)
            //
            let ms = Mouse.GetState()
            if ms.LeftButton = ButtonState.Pressed then
                if startBtnE.isClicked(ms.X, ms.Y) then
                    startBtnE.OnClick()
                else if startBtnM.isClicked(ms.X, ms.Y) then
                    startBtnM.OnClick()
                else if startBtnH.isClicked(ms.X, ms.Y) then
                    startBtnH.OnClick()
            else 
                if startBtnE.isClicked(ms.X, ms.Y) then
                    startBtnE.OnHoverEnter()
                else if startBtnM.isClicked(ms.X, ms.Y) then
                    startBtnM.OnHoverEnter()
                else if startBtnH.isClicked(ms.X, ms.Y) then
                    startBtnH.OnHoverEnter()
                else 
                    startBtnE.OnHoverLeave()
                    startBtnM.OnHoverLeave()
                    startBtnH.OnHoverLeave()

        | Result -> ()
        | Game ->
            let dt = float32( gameTime.ElapsedGameTime.Milliseconds) / 1000.0f
            resetBtn.Update(dt)
            let ms = Mouse.GetState()
            if ms.LeftButton = ButtonState.Pressed then
                if resetBtn.isClicked(ms.X, ms.Y) then
                    resetBtn.OnClick()
                    turn <- SquareState.WhiteCircle
                    squareItemSeq <- seq { for index in 0 .. 63 do
                                    let row = index / 8
                                    let col = index % 8
                                    if (col % 2) + (row % 2) = 1 then
                                        yield new SquareItem.Square(gsquare, wcircle, bcircle, 100 + (50 * col), 100 + (50 * row),col, row)
                                    else
                                        yield new SquareItem.Square(gsquare, wcircle, bcircle, 100 + (50 * col), 100 + (50 * row), col, row )
                            }
            else 
                if resetBtn.isClicked(ms.X, ms.Y) then
                    resetBtn.OnHoverEnter()
                else 
                    resetBtn.OnHoverLeave()

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

                    // The minimax evaluation function -
                    // This is the function used to distinguish
                    // good moves from bad mvoes for the CPU player.
                    let geval (board : Square seq, color : SquareState) =
                        seq { for item in board do
                                       if item.GetSquareState() = color then
                                            let scre = match (item.col, item.row) with
                                                       | (0,0) | (0, 7) | (7, 0) | (7,7) -> 45
                                                       | (0,_) | (7, _) | (_, 0) | (_,7) -> 1
                                                       | _ -> 1
                                            yield scre
                                       else if item.GetSquareState() = GameBoard.GetEnemyColor color then
                                            let scre = match (item.col, item.row) with
                                                       | (0,0) | (0, 7) | (7, 0) | (7,7) -> -45
                                                       | (0,_) | (7, _) | (_, 0) | (_,7) -> -1
                                                       | _ -> -1
                                            yield scre
                                       else yield 0
                                       } |> Seq.reduce (+)

                    let f board =
                        Seq.filter ( fun (a : Square) -> a.GetSquareState() = SquareState.BlackCircle) board |> Seq.length

                    let mm = new MinimaxController(depth, fun (brd : Square seq, colr : SquareState) -> geval(brd, colr))

                    let score = mm.Evaluate(squareItemSeq, SquareState.BlackCircle)
                    match score with
                    | None -> ()
                    | Some sc -> PutColor(&squareItemSeq, sc.Position.col, sc.Position.row, SquareState.BlackCircle)

                    turn <- SquareState.WhiteCircle
            | _ -> ()


    override Game.Draw gameTime =
        this.GraphicsDevice.Clear(Color.CornflowerBlue)
        spriteBatch.Begin()
        spriteBatch.DrawString(Font1, "Othello XNA!",  new Vector2(10.0f, 10.0f), Color.White)
        match state with
        | Game ->
            Seq.iter (fun (sq : Square) -> sq.draw spriteBatch) squareItemSeq |> ignore
            resetBtn.Draw(spriteBatch)
            if turn = SquareState.WhiteCircle then
                spriteBatch.DrawString(Font1, "Player turn...",  new Vector2(250.0f, 10.0f), Color.White)
            else if turn = SquareState.BlackCircle then
                spriteBatch.DrawString(Font1, "AI turn...",  new Vector2(250.0f, 10.0f), Color.White)
            else
                match this.CalculateWinner() with
                        | SquareState.WhiteCircle -> spriteBatch.DrawString(Font1, "Player wins!",  new Vector2(250.0f, 10.0f), Color.White)
                        | SquareState.BlackCircle -> spriteBatch.DrawString(Font1, "AI win!",  new Vector2(250.0f, 10.0f), Color.Black)
                        | _ -> spriteBatch.DrawString(Font1, "No Winner",  new Vector2(250.0f, 10.0f), Color.Gray)
        | Home ->
            
            startBtnE.Draw(spriteBatch)
            startBtnM.Draw(spriteBatch)
            startBtnH.Draw(spriteBatch)
        | Result ->
            ()
        spriteBatch.End()
        base.Draw gameTime
