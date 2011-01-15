namespace FSharpMVC2.Web.Controllers

open System.Web
open System.Web.Mvc

[<HandleError>]
type HomeController() =
    inherit Controller()
    member x.Index () : ActionResult =
        x.ViewData.["Message"] <- "Welcome to ASP.NET MVC!"
        x.View() :> ActionResult
    member x.About () =
        x.View() :> ActionResult