
(** L'objectif de la file à priorité est de pouvoir inserer des éléments dans une file, sans pour autant les mettre au début ou à la fin, ici on est dépendant de la distance parcourue et de celle qui reste à parcourir *)

(***)
type ('a,'b) t
(** Création du type file à priorité *)
  
val empty : ('a,'b)t
(** Initialisation de la file vide *)

val is_empty: ('a,'b)t -> bool
(** Vérification de file vide, True si oui False sinon *)
  
val cardinal: ('a,'b)t -> int
(** Nombre d'éléments de la pile *)

val insert : 'a -> 'b -> ('a,'b) t -> ('a,'b)t
(** Insert un élément dans la pile à la place voulu, en utilisant la fonction de comparaison voulue (ici on compare soit f soit g)  et retourne la pile mise à jour *)

val extract: ('a,'b)t -> ('a * 'b * ('a,'b) t)
(** Enlève un élément dans la pile et retourne les informations le concernant ( f, h, coordonnées, ....) ainsi que la nouvelle file *)

val iter: ('a -> 'b -> unit) -> ('a,'b) t -> unit
(** Permet d'applique une fonction à tout les éléments de la pile en fonction de la priorité *)
  
val elements : ('a,'b) t -> 'b list
(** retourne les éléments de la pile dans l'ordre de priorité *)
