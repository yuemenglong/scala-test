package pack.jvm.nativ

import pack.jvm.rt.Vm

/**
  * Created by <yuemenglong@126.com> on 2018/2/23.
  */
class Str(val inner: String) extends Obj(Vm.rt.load("java/lang/String")) {
  this.set("value", new ArrI(inner.toCharArray))
}
