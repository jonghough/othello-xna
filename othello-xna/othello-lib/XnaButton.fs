module XnaButton

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type ButtonState = | Normal | Hover | Clicked

type Button(x, y, width, height, normalImage, hoverImage, clickedImage, ClickFunc : unit -> unit) =
    let mutable btnState = ButtonState.Normal
    let mutable timer : float32 = 0.0f
    member this.ClickFunc = ClickFunc
    member this.posx : int = x
    member this.posy : int = y
    member this.width : int = width
    member this.height : int = height
    member this.normalImage = normalImage
    member this.hoverImage = hoverImage
    member this.clickedImage = clickedImage

    member public this.isClicked(x : int, y : int) =
         this.posx < x && this.posx + this.width >= x && this.posy < y && this.posy + this.height >= y

    member public this.Update(deltaTime : float32) =
        match btnState with
        
        | Clicked -> 
            timer <- timer - deltaTime
            if timer < 0.0f then
                ClickFunc()
                btnState <- ButtonState.Normal
                System.Diagnostics.Debug.WriteLine("called click func")
        | _ -> ()

    member public this.Draw(spriteBatch : SpriteBatch) =
        match btnState with
        | Normal -> spriteBatch.Draw(normalImage, new Rectangle(this.posx, this.posy,this.width, this.height), Color.White)
        | Hover -> spriteBatch.Draw(hoverImage, new Rectangle(this.posx, this.posy,this.width, this.height), Color.Cornsilk)
        | Clicked -> 
            spriteBatch.Draw(clickedImage, new Rectangle(this.posx, this.posy,this.width, this.height), Color.White)


    member public this.OnClick() =
        match btnState with
        | Normal | Hover ->
            timer <- 0.3f
            btnState <- Clicked

        | _ -> ()   

    member public this.OnHoverEnter() =
        match btnState with
        | Normal ->
            btnState <- Hover

        | _ -> ()

    member public this.OnHoverLeave() =
        match btnState with
        | Hover ->
            btnState <- Normal

        | _ -> ()