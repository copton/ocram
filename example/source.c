void f() {
	stmt1();
	while(1) {
		stmt2();
		blocking();
		stmt3();
	}
	stmt4();
}
