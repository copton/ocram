TIMEOUT(600000);

while (true) {
	YIELD();
	if (msg.startsWith("QUIT")) {
		log.testOK();
	} else if (msg.startsWith("ASSERT")) {
		log.testFailed();
	}
}
