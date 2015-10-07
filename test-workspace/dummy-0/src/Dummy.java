
public class Dummy {

	public static void main(String[] args) {
		try {
			Thread.sleep(10000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
		System.out.println("0");
	}

}
