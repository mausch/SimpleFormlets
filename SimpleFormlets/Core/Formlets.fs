namespace Formlets

type 'a NameGen = int -> 'a * int
module NameGen =
    let puree (v : 'a) : 'a NameGen = 
        fun gen -> v,gen
    let (<*>) (f: ('a -> 'b) NameGen) (a: 'a NameGen) : 'b NameGen = 
        fun gen ->
            let v,gen = f gen
            let w,gen = a gen
            v w, gen
    let lift f a = puree f <*> a
    let lift2 f a b = puree f <*> a <*> b
    let nextName : string NameGen =
        fun gen ->
            "input_" + gen.ToString(), gen+1
    let run (c: 'a NameGen) : 'a = fst (c 0)

type 'a Environ = (string*string) list -> 'a
module Environ =
    let puree v = fun env -> v
    let (<*>) (f: ('a -> 'b) Environ) (a: 'a Environ) : 'b Environ =
        fun env ->
            let g = f env
            g(a(env))
    let lift f a = puree f <*> a
    let lookup (n: string) : string Environ =
        fun env ->
            match List.tryFind (fun (k,_) -> k = n) env with
            | Some (_,v) -> v
            | _ -> failwithf "Key %s not found in environment" n

    open System.Collections.Specialized
    let fromNV (a: NameValueCollection) =
        a.AllKeys
        |> Seq.collect (fun k -> a.GetValues k |> Seq.map (fun v -> k,v))
        |> Seq.toList

type xml_item =
    | Text of string
    | Tag of string * (string*string) list * xml_item list

type 'a XmlWriter = xml_item list * 'a
module XmlWriter =
    let puree (v: 'a) : 'a XmlWriter = [],v
    let (<*>) (f: ('a -> 'b) XmlWriter) (a: 'a XmlWriter) : 'b XmlWriter =
        fst f @ fst a, (snd f) (snd a)
    let lift f a = puree f <*> a
    let lift2 f a b = puree f <*> a <*> b
    let plug (f: xml_item list -> xml_item list) (a: 'a XmlWriter) : 'a XmlWriter =
        f (fst a), snd a
    let xml (e: xml_item list): unit XmlWriter =
        plug (fun _ -> e) (puree ())
    let text (s: string) : unit XmlWriter =
        xml [Text s]
    let tag (t: string) (attr: (string*string) list) (v: 'a XmlWriter) : 'a XmlWriter =
        plug (fun x -> [Tag(t, attr, x)]) v

    open System.Xml.Linq
    let render (xml: xml_item list) : XDocument =
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

module EnvironXmlWriter =
    let puree (v: 'a) : 'a Environ XmlWriter = 
        v |> Environ.puree |> XmlWriter.puree 
    let (<*>) (f: ('a -> 'b) Environ XmlWriter) (a: 'a Environ XmlWriter) : 'b Environ XmlWriter = 
        XmlWriter.lift2 Environ.(<*>) f a
    let lift f a = puree f <*> a
    let refine (x: 'a XmlWriter) : 'a Environ XmlWriter =
        XmlWriter.lift Environ.puree x

type 'a Formlet = 'a Environ XmlWriter NameGen
[<AutoOpen>]
module Formlet =
    let puree v : _ Formlet = v |> EnvironXmlWriter.puree |> NameGen.puree
    let (<*>) (f: ('a -> 'b) Formlet) (a: 'a Formlet) : 'b Formlet =
        NameGen.lift2 EnvironXmlWriter.(<*>) f a
    let lift f a : _ Formlet = puree f <*> a
    let lift2 f a b : _ Formlet = puree f <*> a <*> b
    let ( *>) f a : _ Formlet = lift2 (fun _ z -> z) f a
    let ( <*) f a : _ Formlet = lift2 (fun z _ -> z) f a
    let xml (x: xml_item list) : unit Formlet =
        NameGen.puree (EnvironXmlWriter.refine (XmlWriter.xml x))
    let text (s: string) : unit Formlet =
        xml [Text s]
    let tag (t: string) (attr: (string*string) list) (f: 'a Formlet) : 'a Formlet =
        NameGen.lift (XmlWriter.tag t attr) f
    let input : string Formlet =
        let xml name = XmlWriter.tag "input" ["name",name]
        let lookup name = XmlWriter.puree (Environ.lookup name)
        let tag name = xml name (lookup name)
        NameGen.lift tag NameGen.nextName
    let br: unit Formlet = xml [Tag("br",[],[])]
    let run (f: 'a Formlet) : 'a Environ =
        NameGen.run f |> snd

    open System.Xml.Linq
    let render (f: _ Formlet) = 
        NameGen.run f |> fst |> XmlWriter.render
