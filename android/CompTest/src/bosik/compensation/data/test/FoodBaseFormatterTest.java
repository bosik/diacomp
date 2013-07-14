package bosik.compensation.data.test;

import org.bosik.compensation.persistence.entity.foodbase.FoodBase;
import org.bosik.compensation.persistence.repository.foodbase.FoodBaseFormatter;
import junit.framework.TestCase;

public class FoodBaseFormatterTest extends TestCase
{
	private final String xml = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" + "<foods version=\"167\">\n"
			+ "	<food name=\"јбрикос\" prots=\"0.9\" fats=\"0.1\" carbs=\"9\" val=\"41\" table=\"True\"/>\n"
			+ "	<food name=\"яйцо\" prots=\"12.7\" fats=\"11.5\" carbs=\"0.7\" val=\"157\" table=\"False\"/>\n" + "</foods>";

	public void testGetVersion()
	{
		int version = FoodBaseFormatter.getVersion(xml);
		assertEquals(167, version);
	}

	public void testRead()
	{
		FoodBase base = FoodBaseFormatter.read(xml);

		assertEquals(167, base.getVersion());
		assertEquals(2, base.count());

		assertEquals("јбрикос", base.get(0).getName());
		assertEquals(0.9, base.get(0).getRelProts());
		assertEquals(0.1, base.get(0).getRelFats());
		assertEquals(9.0, base.get(0).getRelCarbs());
		assertEquals(41.0, base.get(0).getRelValue());
		assertEquals(true, base.get(0).getFromTable());

		assertEquals("яйцо", base.get(1).getName());
		assertEquals(12.7, base.get(1).getRelProts());
		assertEquals(11.5, base.get(1).getRelFats());
		assertEquals(0.7, base.get(1).getRelCarbs());
		assertEquals(157.0, base.get(1).getRelValue());
		assertEquals(false, base.get(1).getFromTable());
	}
}
