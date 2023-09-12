package object FuncionesRecursivas {
  def tamI(l: List[Int]): Int = {
    def tamC(lista: List[Int], contador: Int): Int={
      if(lista.isEmpty) contador
      else tamC(lista.tail, contador + 1)
    }

    tamC(l,0)
  }

  def menoresQue(l:List[Int], v: Int): List[Int] ={
    if(l.isEmpty) Nil
    else {
      if(l.head < v) l.head::menoresQue(l.tail, v)
      else menoresQue(l.tail, v)
    }
  }

  def noMenoresQue(l: List[Int], v: Int): List[Int] = {
    if(l.isEmpty) Nil
    else{
      if(l.head > v) l.head::noMenoresQue(l.tail, v)
      else noMenoresQue(l.tail, v)
    }
  }

  def k_elem(l:List[Int], k: Int): Int ={
    if(l.isEmpty) 0
    else{
      if(k < 0 || k >= tamI(l)) 0
      else l(k)
    }
  }


  def ordenar(l:List[Int]): List[Int] = {
    if (l.isEmpty) Nil
    else {
      ordenar(menoresQue(l.tail, l.head)) ++ List(l.head) ++ ordenar(noMenoresQue(l.tail, l.head))
    }
  }
}
