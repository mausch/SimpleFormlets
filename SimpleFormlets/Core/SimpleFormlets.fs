namespace SimpleFormlets

type xml_item =
    | Text of string
    | Tag of string * (string*string) list * xml_item list

type Env = (string*string) list

type 'a Formlet = int -> (xml_item list * (Env -> 'a) * int)

[<AutoOpen>]
module Formlet =
    let puree (v: 'a) : 'a Formlet = 
        fun i -> [], (fun _ -> v), i
    let (<*>) (f: ('a -> 'b) Formlet) (a: 'a Formlet) : 'b Formlet =
        fun i -> 
            let x1,g,i = f i
            let x2,q,i = a i
            x1 @ x2, (fun env -> g env (q env)), i
    let lift f a = puree f <*> a
    let lift2 f a b : _ Formlet = puree f <*> a <*> b
    let ( *>) f a : _ Formlet = lift2 (fun _ z -> z) f a
    let ( <*) f a : _ Formlet = lift2 (fun z _ -> z) f a
    let xml (x: xml_item list) : unit Formlet =
        fun i -> x, (fun _ -> ()), i
    let text (t: string) : unit Formlet =
        xml [Text t]
    let tag (name: string) (attr: (string*string) list) (f: 'a Formlet) : 'a Formlet =
        fun i ->
            let xml,env,i = f i
            [Tag(name, attr, xml)],env,i
    let nextName =
        fun i -> "input_" + i.ToString(), i+1
    let input : string Formlet =
        fun i ->
            let name,i = nextName i
            let lookup = List.find (fun (k,_) -> k = name) >> snd
            let tag = Tag("input", ["name",name], [])
            [tag], lookup, i
    let br: unit Formlet = xml [Tag("br",[],[])]
    let run (f: 'a Formlet) : Env -> 'a =
        let _,e,_ = f 0
        e

    open System.Xml.Linq
    let render (f: 'a Formlet) : XDocument =
        let xml,_,_ = f 0
        let (!!) t = XName.op_Implicit t
        let xtext (s: string) = XText s :> XObject
        let xattr (name, value) = XAttribute(!!name, value) :> XObject
        let xattrs attr = List.map xattr attr
        let xelem name attr children = XElement(!!name, attr @ children) :> XObject
        let rec renderForest x =
            let render' =
                function
                | Text t -> xtext t
                | Tag(name, attr, children) -> xelem name (xattrs attr) (renderForest children)
            List.map render' x
        let root = xelem "div" [] (renderForest xml)
        XDocument root

    open System.Collections.Specialized
    let fromNV (a: NameValueCollection) =
        a.AllKeys
        |> Seq.collect (fun k -> a.GetValues k |> Seq.map (fun v -> k,v))
        |> Seq.toList
        