TIMEOUT(600000);

while (true) {
	WAIT_UNTIL(msg.startsWith("QUIT"));
	log.testOK();
}
