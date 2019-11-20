class ['a] army (l:'a list) =
  object
    val _l = l

    method add e = new army (e::l)
    method delete = new army (List.tl l)

    method get_l = l
  end
