module Minimax
open SquareItem
open GameBoard

type ScoreCouple = {
        Position : Square;
        Score : int option;
    }

type MinimaxController(depth : int, eval : Square seq * SquareState -> int) =
    member this.maxDepth = depth
    member this.evaluationFunc = eval

   

    ///<summary>
    /// Changes the square state of the given board, by reference.
    ///</summary>
    member this.ChangeSquareState(board : Square seq byref, color : SquareState, i, j) =
        PutColor(&board,i,j,color)


    ///<summary>
    /// Runs th enext iteration of Minimax, i.e. goes one node lower down the game tree.
    ///</summary>
    member private this.RunNext(board : Square seq, color : SquareState, depth : int, alpha : int byref, beta : int byref, isMaximizing : bool, possibleMoves: Square seq)  =

       // let mutable nextAlpha = alpha
        let mutable highScore = alpha
        let nextMaximizing = if isMaximizing = true then false else true
        if isMaximizing = false then
            highScore <- beta
        //let mutable nextBeta = beta

        let mutable quit = false
        for i in 0 .. ((Seq.length possibleMoves) - 1) do
            if quit = false then
                if isMaximizing = true then
                    let mutable boardCopy = board
                    let scr = this.RunMinimax(&boardCopy, color, Seq.nth i possibleMoves, depth, &alpha, &beta, isMaximizing)
                    match scr with
                    | None -> ()
                    | Some eval ->
                        System.Diagnostics.Debug.WriteLine("score is " + string(eval)+", depth+ = "+string(depth) + "alpha beta " + string(alpha) + ", " + string(beta))
                        if eval >= highScore then
                            highScore <- eval
                        if alpha <= eval then 
                            alpha <- eval
                        if beta <= alpha then
                            highScore <- alpha 
                            quit <- true
                else
                    let mutable boardCopy = board
                    let scr = this.RunMinimax(&boardCopy, color, Seq.nth i possibleMoves, depth, &alpha, &beta, isMaximizing)
                    match scr with 
                    | None -> ()
                    | Some eval ->
                        System.Diagnostics.Debug.WriteLine("score is " + string(eval)+", depth- = "+string(depth) + "beta and alpha " + string(beta) + ", " + string(alpha))
                        let negEval =  eval
                        if negEval <= highScore then
                            highScore <- negEval
                        if beta >= negEval then 
                                beta <- negEval
                        if beta <= alpha then
                            highScore <- beta 
                            quit <- true
        Some highScore    

    ///<summary>
    /// Runs the minimax aglorithm. If the maximum depth is reached, then the evaluation function is invoked and the result for this game
    /// tree leaf node is calculated, else the next level of the game tree is investigated, where color and isMaximizing are
    /// changed.
    ///</summary>
    member private this.RunMinimax (board : Square seq byref, color : SquareState, position : Square, depth : int, alpha : int byref, beta : int byref, isMaximizing : bool) : int option =
        this.ChangeSquareState(&board, color, position.col, position.row)
        if depth >= this.maxDepth then
            this.evaluationFunc(board, SquareState.BlackCircle) |> Some
            
        else
            let nextMaximizing = if isMaximizing = true then false else true
            let possibleMoves = GetPossibleMoves(board,(GetEnemyColor color))
            this.RunNext(board, (GetEnemyColor color), depth + 1, &alpha, &beta, nextMaximizing, possibleMoves)
            

    ///<summary>
    ///  Initializes the minimax game tree search.
    ///</summary>
    member public this.Evaluate(board : Square seq, color : SquareState) : ScoreCouple option =
        let possibleInitialMoves = GetPossibleMoves(board, color)
        if Seq.isEmpty possibleInitialMoves then
            None
        else
            let optimalScores : ScoreCouple seq = seq { for sq in possibleInitialMoves do
                                                            let mutable bc = board
                                                            let alpha = ref -100000
                                                            let beta = ref 100000
                                                            let bestScore = this.RunMinimax(&bc, color, sq, 0, alpha, beta, true)
                                                            yield {Position = sq; Score = bestScore} }

            let opt = Seq.maxBy (fun (s : ScoreCouple) -> s.Score ) optimalScores
            System.Diagnostics.Debug.WriteLine("optimal " + string(opt.Position.col) + ", " + string(opt.Position.row) + " : score " + string(opt.Score))
            Some opt

        
    