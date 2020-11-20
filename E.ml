open Array
open String
open Bytes

(*estrutura para o tipo null*)
type null =
  {
    nuTer:char;
    nu:bool
  }
(*estrutura para o tipo first*)
type first =
  {
    frTer:char;
    fr:string
  }
(*estrutura para o tipo follow*)
type follow =
  {
    foTer:char;
    fo:string
  }
(*estrutura para o tipo linha para as produções da gramática*)
type line =
  {
    nonTer:char;
    prod:string
  }
(*cria uma nova linha/produção*)
let createLine a b =
  {
    nonTer = a;
    prod = b
  }
(*cria uma nova null na forma (N, resultado)*)
let createNull c d =
  {
    nuTer = c;
    nu = d
  }
(*cria uma nova first na forma (N, resultado)*)
let createFirst c d =
  {
    frTer = c;
    fr = d
  }
(*cria uma follow null na forma (N, resultado)*)
let createFollow c d =
  {
    foTer = c;
    fo = d
  }
(*cria uma nova gramática formada por n linhas/produções*)
let createGrammar a b n =
  let gram = Array.make n (createLine ' ' "") in
    for i=0 to n-1 do
      gram.(i)<-createLine a.(i) b.(i) (*adiciona a produção à gramática*)
    done;
  gram (*devolve a gramática*)

let isTerminal alpha = (*verifica se a 'letra' é terminal ou não*)
  match alpha with
  | 'A' .. 'Z' -> false
  | '_' -> false
  | _ -> true;;

let rec hasTerminal alpha = (*verifica se uma string contém um terminal*)
  if String.length alpha = 0 then false (*se a string for vazia devolve false*)
  else if isTerminal alpha.[0] = true then true (*se encontra um terminal devolve true*)
  else false || (hasTerminal (String.sub alpha 1 (String.length alpha - 1))) (*se não, devolve false*)

let rec hasNonTerminal alpha = (*verifica se uma string contém um não-terminal*)
  if String.length alpha = 0 then false (*se a string for vazia devolve false*)
  else if isTerminal alpha.[0] = false then true (*se encontra um não-terminal devolve true*)
  else false || (hasNonTerminal (String.sub alpha 1 (String.length alpha - 1))) (*se não, devolve false*)

let removeSpacesFromString str = (*remove os espações em branco de uma string*)
  let retStr = ref "" in
  for i=0 to (String.length str) - 1 do
    if str.[i] <> ' ' then retStr:= !retStr^(String.make 1 str.[i]) (*se o valor for diferente de ' ' é adicionado à string que vai ser retornada*)
  done;
  !retStr (*retorna a string sem espaços*)

let sortOutput s = (*ordena uma string por ordem alfabética*)
  let n = String.length s in
  let a = Array.init n (fun i -> s.[i]) in
  Array.sort Char.compare a;
  String.init n (fun i -> a.(i))

let removeDuplicates str = (*remove chars duplicados de uma string*)
  let a = ref (Array.make 0 ' ') in
  let outStr = ref "" in
  let tempStr = ref "" in
  for i=0 to (String.length str) - 1 do
    if String.length str = 1 && str = "_" then outStr:=str (*se o conteúdo da string for '_' não faz nada*)
    else if Array.mem (str.[i]) !a = false && str.[i] <> '_' then a:= Array.append !a (Array.make 1 str.[i]) (*se o char ainda não existr em 'a' e for diferente de '_', é adicionado a 'a'*)
  done;
  for i=0 to (Array.length !a) - 1 do
    tempStr:= !tempStr^(String.make 1 !a.(i)) (*passa os valores  do array para string*)
  done;
  tempStr:= sortOutput (!tempStr); (*a string é ordenada*)
  for i=0 to (String.length !tempStr) - 1 do (*adiciona os espaços entre os char para serem impressos*)
    if i = 0 then outStr:= !outStr^(String.make 1 !tempStr.[i])
    else outStr:= !outStr^" "^(String.make 1 !tempStr.[i])
  done;
  (!outStr, !tempStr) (*revolve a string com os espaços e a string sem os espaços*)


let removeEps str = (*remove '_' da string*)
  let strByte = Bytes.of_string str in
  if Bytes.contains strByte '_' = true then Bytes.to_string (Bytes.sub strByte 1 ((Bytes.length strByte) - 1))
  else str


let rec null g n beta = (*calcula null*)
  let nullable = ref false in
  for i=0 to n-1 do (*itera sobre as produções da gramática*)
    if g.(i).nonTer = beta then begin (*se a produção tiver 'beta' á esquerda*)
      if g.(i).prod = "_" then nullable:= true (*se alfa = _ então true*)
      else if hasTerminal g.(i).prod = true then nullable:= !nullable || false (*se contém um não terminal, adiciona 'ou false'*)
      else begin (*se não*)
        nullable := true;
        for j=0 to (String.length g.(i).prod) -1 do (*precorre a parte direita*)
          if String.contains g.(i).prod beta = true then nullable:= false (*se contém beta, então false*)
          else begin (*se não*)
            if isTerminal g.(i).prod.[j] = false then begin (*se não é terminal*)
              if null g n g.(i).prod.[j] = true then nullable:= !nullable && true (*se null de g.(i).prod.[j] = true, então true*)
              else nullable:= !nullable && false (*se não, false*)
            end
          end
        done;
      end
    end
  done;
  !nullable (*devolve o resultado*)


let rec first g n nuArray beta str = (*calcula first*)
  if isTerminal beta = true then str:= String.make 1 beta (*se 'beta' for terminal, é adicionado ao resultado*)
  else begin (*se não*)
    for i=0 to n-1 do (*itera sobre as produções da gramática*)
      if g.(i).nonTer = beta && g.(i).nonTer <> g.(i).prod.[0] then begin
        if g.(i).prod = (String.make 1 '_') then str:= !str^(String.make 1 '_') (*se a parte direita for '_' e adicionado ao resultado*)
        else if hasNonTerminal (g.(i).prod) = false then str:= !str^(String.make 1 g.(i).prod.[0]) (*se a parte direita não contém terminais g.(i).prod.[0] é adicionado ao resultado*)
        else if isTerminal g.(i).prod.[0] = true then str:= !str^(String.make 1 g.(i).prod.[0]) (*se g.(i).prod.[0] for terminal, g.(i).prod.[0] é adicionado ao resultado*)
        else if isTerminal g.(i).prod.[0] = false then begin (*se g.(i).prod.[0] não for terminal*)
          for j=0 to (Array.length nuArray) -1 do (*itera sobre o array dos null*)
            if nuArray.(j).nuTer = g.(i).prod.[0] && nuArray.(j).nu = false then str:= !str^(first g n nuArray g.(i).prod.[0] str) (*se null(g.(i).prod.[0]) for false, então é adicionado first(g.(i).prod.[0]) ao resultado*)
            else if nuArray.(j).nuTer = g.(i).prod.[0] && nuArray.(j).nu = true && String.length g.(i).prod = 1 then str:= !str^(first g n nuArray g.(i).prod.[0] str) (*se null(g.(i).prod.[0]) for true e String.length g.(i).prod = 1, então é adicionado first(g.(i).prod.[0]) ao resultado*)
            else if nuArray.(j).nuTer = g.(i).prod.[0] && nuArray.(j).nu = true && String.length g.(i).prod > 1 then begin (*se null(g.(i).prod.[0]) for true e String.length g.(i).prod = 1, então ...*)
              let tempStr = ref "" in
              for q=0 to (String.length g.(i).prod) -1 do (*precorre a parte direita da produção*)
                if g.(i).prod.[q] <> beta then tempStr:= !tempStr^(first g n nuArray g.(i).prod.[q] str) (*se g.(i).prod.[q] não for igual a beta, é adicionado g.(i).prod.[q] ao resultado*)
              done;
              str:= !str^ !tempStr (*ao resultado é concatenado tempStr calculado anteriormente*)
            end
          done;
        end
      end
    done;
  end;
  !str (*devolve o resultado*)


let rec follow g n firArray fn nuArray nn folArray beta str = (*calcula follow*)
  let pos = ref 0 in (*guarda o valor da posição de beta*)
  if beta = 'S' then str:= "#"; (*se beta for o símbolo inicial, adicoina # o resultado*)
  for i=0 to n-1 do (*itera sobre as produções da gramática*)
    if String.contains g.(i).prod beta = true then begin (*se a parte direita da produção contém beta*)
      for j=0 to (String.length g.(i).prod) -1 do
        if g.(i).prod.[j] = beta then begin (*se g.(i).prod.[j] for igual a beta*)
          pos:= j; (*guarda-se o valor da sua posição*)
          if j = (String.length g.(i).prod) -1 then begin (*se beta for o char mais à direita*)
            if g.(i).prod.[j] <> g.(i).nonTer then begin (*se g.(i).prod.[j], for diferente da parte esuqrda da produção*)
              for p=0 to (Array.length !folArray) -1 do (*precorre o array dos follow*)
                if !folArray.(p).foTer = g.(i).nonTer then str:= !str^(!folArray.(p).fo) (*se encontrar g.(i).nonTer no array, adiciona o valor de Follow(g.(i).nonTer) ao resultado*)
              done;
            end
          end
          else begin
            let tempStr = ref "" in (*string trmporária*)
            let flag = ref true in (*flag de null*)
            for p= !pos+1 to (String.length g.(i).prod) -1 do (*precorre a parte direita da produção, apartir da posição à direta de beta*)
              if !flag = true then begin
                tempStr:= !tempStr^(first g n nuArray g.(i).prod.[p] (ref "")); (*adiciona First(g.(i).prod.[p]) a tempStr*)
                if isTerminal g.(i).prod.[p] = false then begin (*se g.(i).prod.[p] não for terminal*)
                  if null g n g.(i).prod.[p] = true then flag:= !flag && true (*se null for true, a flag fica ''&& true'*)
                  else if null g n g.(i).prod.[p] = false then flag:= false (*se não, a flag fica false*)
                end
                else flag:= false; (*se não, a flag fica false*)
              end
            done;
            if !flag = false then str:= !str^ !tempStr (*se a flag estiver a false, tempStr é concatenada ao resultado*)
            else begin (*se não*)
              if g.(i).nonTer <> beta then str:= !str^(!tempStr)^(follow g n firArray fn nuArray nn folArray g.(i).nonTer str) (*se g.(i).nonTer for diferente de beta, ao resultado é concatenado 'tempStr U Follow(g.(i).nonTer)'*)
              else str:= !str^(!tempStr) (*se não, é só concatendado tempStr ao resultado*)
            end
          end
        end
      done;
    end
  done;
  folArray:= Array.append !folArray (Array.make 1 (createFollow beta !str)); (*o resultado de follow obtido é adicionado ao array dos follow*)
  !str (*devolve o resultado*)


(*---------- MAIN -----------*)

(*input*)
let () =
let n = read_int() in (*guarda o número de produções da gramática*)
let a = Array.make n ' ' in (*guarda o 'N' da notação N -> a*)
let b = Array.make n "" in (*guarda o 'a' da notação N -> a*)
for i=0 to n-1 do
  let s = read_line() in
    a.(i)<-s.[0];
    b.(i)<-removeSpacesFromString (String.sub s 5 (String.length s - 5)) (*remove os espaços da string para a tornar mais curta*)
done;
let gram = createGrammar a b n in (*cria a gramática*)


(*NULL*)
let prev = ref ' ' in (*valor calculado anteriormente*)
let nuArray = ref (Array.make 0 (createNull ' ' false)) in (*array que guarda os valores NULL da gramática*)

for i=0 to n-1 do
  if gram.(i).nonTer <> !prev then begin (*se o 'N' atual for diferente do anterior*)
    prev:=gram.(i).nonTer; (*atualiza o valor*)
    if null gram n gram.(i).nonTer = true then begin (*se o resultado devolvido pela função null for true*)
      print_endline("NULL("^(String.make 1 gram.(i).nonTer)^") = True"); (*imprime o resultado na forma NULL(N) = True*)
      nuArray:= Array.append !nuArray (Array.make 1 (createNull gram.(i).nonTer true)) (*guarda o calor calculado no array*)
    end
    else begin (*se o resultado devolvido pela função null for true*)
      print_endline("NULL("^(String.make 1 gram.(i).nonTer)^") = False"); (*imprime o resultado na forma NULL(N) = False*)
      nuArray:= Array.append !nuArray (Array.make 1 (createNull gram.(i).nonTer false)) (*guarda o calor calculado no array*)
    end
  end
done;


(*FIRST*)
let str = ref "" in (*string para guardar o resultado*)
let firArray = ref (Array.make 0 (createFirst ' ' "")) in (*array que guarda os valores FIRST da gramática*)
prev:= ' '; (*valor calculado anteriormente*)
for i=0 to n-1 do
  if gram.(i).nonTer <> !prev then begin
    str:= ""; (*"limpa" a string do resultado*)
    let (result_a, result_b) = removeDuplicates(first gram n !nuArray gram.(i).nonTer str) in (*remove chars duplicados na string*)
      print_endline("FIRST("^(String.make 1 gram.(i).nonTer)^") = "^(result_a)); (*imprime o resultado na forma First(N) = string*)
      firArray:= Array.append !firArray (Array.make 1 (createFirst gram.(i).nonTer result_b)); (*guarda o calor calculado no array*)
      prev:=gram.(i).nonTer (*atualiza o valor*)
  end
done;


(*FOLOW*)
let str = ref "" in (*string para guardar o resultado*)
let folArray = ref (Array.make 0 (createFollow ' ' "")) in (*array que guarda os valores FOLLOW da gramática*)
prev:= ' '; (*valor calculado anteriormente*)
for i=0 to n-1 do
  if gram.(i).nonTer <> !prev then begin
    str:= ""; (*"limpa" a string do resultado*)
    let (result_a, result_b) = removeDuplicates(follow gram n !firArray (Array.length !firArray) !nuArray (Array.length !nuArray) folArray gram.(i).nonTer str) in (*remove chars duplicados na string*)
      print_endline("FOLLOW("^(String.make 1 gram.(i).nonTer)^") = "^(result_a)); (*imprime o resultado na forma Follow(N) = string*)
      prev:=gram.(i).nonTer (*atualiza o valor*)
  end
done;



(*
  Dada a quantidade de casos possíveis inerentes ao problema é mais fácil explicar um caso geral do que um concreto.
  O programa recebe os valores de input, que consistem no número de produções, seguido das respetivas produções.
  Depois de terminado o input, as produções são transformadas em 'linha' (esturutura que guarda as produções na forma N-> alfa). Para diminuir o tamanho de alfa são eliminados os espaços em branco entre chars
  É calculado Null e impresso o seu respetivo valor. Os valores calculados são guardados no array dos null para serem usados futuramente, evitando o recalcular de resultados já calculados.
  O mesmo processo de calcular, guradar e imprimir é aplicado ao first e follow.

  As funções usadas para calcular Null First e Follow, resumem-se a:

  Null:
  NULL(A) = NULL(a1) V ... V NULL(an)
  Null(a) =
        -> true se a = _
        -> NULL(X1) ^ : : : ^ NULL(Xm) se se a = X1 e ... e Xm para qualquer valore de 1 a m e Xi pertencente a N
        -> false senão


  First:
  FIRST(A) = FIRST(a1) U ... U FIRST(an)
  FIRST(A) =
        -> _ se a = _
        -> a1 se a for formado por apenas não terminais
        -> First(a1) se a=a1Xa2
        -> First(X) se a=Xa2, Null(X) = false
        -> First(X) U First(a2) se a=Xa2, Null(X) = true

  Follow:
      -> # se S
      -> Follow(A) se A -> aB

      -> se A -> aBb
          Follow(B) = First(b) se Null(b) = false
          Follow(B) = (First(b) - _) U Follow(A) se Null(b) = true



        Assim, para a entrada:

        11
        S -> A
        S -> B D e
        A -> A e
        A -> e
        B -> d
        B -> C C
        C -> e C
        C -> e
        C -> _
        D -> a D
        D -> a

        resulta:

        NULL(S) = False
        NULL(A) = False
        NULL(B) = True
        NULL(C) = True
        NULL(D) = False
        FIRST(S) = a d e
        FIRST(A) = e
        FIRST(B) = d e
        FIRST(C) = e
        FIRST(D) = a
        FOLLOW(S) = #
        FOLLOW(A) = # e
        FOLLOW(B) = a
        FOLLOW(C) = a e
        FOLLOW(D) = e

*)



(*
https://www.geeksforgeeks.org/program-calculate-first-follow-sets-given-grammar/
https://www.gatevidyalay.com/first-and-follow-compiler-design/
https://www.di.ubi.pt/~desousa/DLPC/aula_dlpc5-pp.pdf
http://www.di.ubi.pt/~desousa/TC/aula_tc5-pp.pdf
exercícios das aulas práticas e teóricas
esclarecimento de dúvidas
*)
