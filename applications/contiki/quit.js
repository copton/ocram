TIMEOUT(700000);

while (true) {
	YIELD();
	if (msg.startsWith("QUIT")) {
		log.testOK();
	}
}
