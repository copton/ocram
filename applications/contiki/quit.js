TIMEOUT(600000);

while (true) {
	YIELD();
	if (msg.startsWith("QUIT")) {
		log.testOK();
	}
}
