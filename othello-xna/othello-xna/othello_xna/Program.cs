using Microsoft.Xna.Framework.Graphics;
using System;
using othello_lib;
using static OthelloGame;
namespace othello_xna
{
#if WINDOWS || XBOX
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        static void Main(string[] args)
        {

            using (OthelloGame.Gamex game = new OthelloGame.Gamex())
            {
                //Content.Load<SpriteFont>("Segoe UI Mono")
                game.Run();
            }
        }
    }
#endif
}

