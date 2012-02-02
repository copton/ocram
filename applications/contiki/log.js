TIMEOUT(300000);

while (true) {
  log.log(time + "\tID:" + id + "\t" + msg + "\n");
//  log.log("XXX" + mote.getCPU().reg[0] + "\n");
  YIELD();
}
