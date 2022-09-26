[<AutoOpen>]
module Pipelets

open System


module Result =

    let ofOption e =
        function
        | Some x -> Ok x
        | None -> Error e

    let toOption =
        function
        | Ok x -> Some x
        | Error _ -> None

    let either ok error =
        function
        | Ok x -> ok x
        | Error _ -> error

    let fromByRef e =
        function
        | true, x -> Ok x
        | _ -> Error e

    let tryTo f x =
        try
            f x |> Ok
        with
        | e -> Error e.Message


module String =

    open System.Text.RegularExpressions

    let toUpper (s: string) =
        if String.IsNullOrEmpty s then
            s
        else
            s.ToUpper()

    let toLower (s: string) =
        if String.IsNullOrEmpty s then
            s
        else
            s.ToLower()

    let toTitleCase (s: string) =
        if String.IsNullOrEmpty s then
            s
        else
            Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase s

    let ifNullOrEmpty f1 f2 s =
        if String.IsNullOrEmpty s then
            f1 s
        else
            f2 s

    let isNullOrEmptyR desc s =
        if String.IsNullOrEmpty s then
            sprintf "%s is null or empty" desc |> Error
        else
            Ok s

    let trim (s: string) =
        if String.IsNullOrEmpty s then
            s
        else
            s.Trim()

    let contains (needle: string) (haystack: String) =
        if String.IsNullOrEmpty haystack then
            false
        else
            haystack.Contains needle

    let private regexWhitespace = new Regex(@"\s+")

    let removeWhitespace (s: String) =
        if String.IsNullOrEmpty s then
            s
        else
            regexWhitespace.Replace(s, String.Empty)

    let split (c: Char) (s: String) =
        if isNull s then
            []
        else
            s.Split c |> List.ofArray

    let caseInsensitiveEquals a b =
        String.Equals(a, b, StringComparison.InvariantCultureIgnoreCase)

    let toOption s =
        if String.IsNullOrEmpty s then
            None
        else
            Some s


module Option =

    let toResult e =
        function
        | Some x -> Ok x
        | None -> Error e

    let fromByRef =
        function
        | true, g -> Some g
        | _ -> None

    let apply f m =
        match f, m with
        | Some unwrappedF, Some unwrappedA -> Some(unwrappedF unwrappedA)
        | _ -> None

    let (<!!>) = Option.map
    let (<**>) = apply

    let mapDefault f none =
        Option.map f >> Option.defaultValue none

module Guid =

    let parseToOption (s: String) = Guid.TryParse s |> Option.fromByRef

    let parseToResult e (s: String) = Guid.TryParse s |> Result.fromByRef e

    let new_ () = Guid.NewGuid()


module DateTime =

    let parseToResult e (date: String) =
        date
        |> DateTime.TryParse
        |> Result.fromByRef (sprintf "%s: %s" e (string date))

    let format (s: string) (d: DateTime) = d.ToString s

    let iso = format "s"

    let isoOffset d = format "s" d + format "zzz" d

    let epochTime (d: DateTime) =
        Convert.ToInt32((d - DateTime.UnixEpoch).TotalSeconds)

    let add (ts: TimeSpan) (d: DateTime) = d.Add ts


module Int32 =

    let parseToResult e (s: String) = Int32.TryParse s |> Result.fromByRef e

    let positiveClamp (a: int) b = Math.Clamp(b, 0, a)


module Boolean =

    let parseToResult e (s: String) =
        Boolean.TryParse s |> Result.fromByRef e

    let parseToOption (s: String) = Boolean.TryParse s |> Option.fromByRef


module Decimal =

    let parseToResult e (s: String) =
        Decimal.TryParse s |> Result.fromByRef e

    let parseSafely default_ (s: String) =
        parseToResult "" s |> Result.either id default_


module Dictionary =

    open System.Collections.Generic

    let tryGetToResult key (d: Dictionary<_, _>) =
        d.TryGetValue key
        |> Result.fromByRef (sprintf "could not get %A from dictionary" key)

    let tryGetToOption key (d: Dictionary<_, _>) = d.TryGetValue key |> Option.fromByRef


module List =

    let tryMin =
        function
        | [] -> None
        | xs -> Some(List.min xs)

    let tryMax =
        function
        | [] -> None
        | xs -> Some(List.max xs)

    let tryHeadR e =
        function
        | h :: _ -> Ok h
        | _ -> Error e


module Map =

    let keyToLowercase (m: Map<String, 'a>) =
        Map.toList m
        |> List.map (fun (k, v) -> k.ToLower(), v)
        |> Map.ofList

    let tryFindR k =
        Map.tryFind k
        >> (Option.toResult
            <| sprintf "Could not find key: %s" k)


module Print =
    let printx x =
        printfn "%A" x
        x


module NonEmptyString =

    type NonEmptyString = private NonEmptyString of String

    module NonEmptyString =

        let create s =
            if String.IsNullOrEmpty s then
                None
            else
                NonEmptyString s |> Some

        let unwrap (NonEmptyString s) = s


module Stream =

    open System.IO

    let toString (stream: Stream) =
        use sr = new StreamReader(stream)
        sr.ReadToEnd()
