namespace FSharpMVC2.Web.Controllers

open System
open System.Collections.Generic
open System.Dynamic
open System.Web
open System.Web.Mvc
open Formlets

[<HandleError>]
type HomeController() =
    inherit Controller()

    let inputInt = lift int input
    let inputDate =
        puree (fun month day year -> DateTime(year,month,day))
        <*> text "Month: " *> inputInt
        <*> text "Day: " *> inputInt
        <*> text "Year: " *> inputInt
    let formlet = 
        puree (fun name order ship amount -> name,order,ship,amount)
        <*> text "Name: " *> input <* br
        <*> text "Order date: " *> inputDate <* br
        <*> text "Shipping date: " *> inputDate <* br
        <*> text "Qty: " *> inputInt <* br

    member x.Index() =
        x.ViewData.["Message"] <- "Welcome to ASP.NET MVC!"
        x.View(box <| render formlet) :> ActionResult
    member x.Register() =
        let env = Environ.fromNV x.Request.Form
        let name,orderDate,shippingDate,amount = run formlet env
        let model = ExpandoObject() :> IDictionary<string, obj>
        model.["Name"] <- name
        model.["Ordered"] <- orderDate
        model.["Shipping"] <- shippingDate
        model.["Amount"] <- amount
        x.View(model) :> ActionResult
    member x.About() =
        x.View() :> ActionResult