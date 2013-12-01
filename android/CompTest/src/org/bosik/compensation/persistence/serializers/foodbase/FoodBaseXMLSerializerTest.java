package org.bosik.compensation.persistence.serializers.foodbase;

import junit.framework.TestCase;
import org.bosik.compensation.bo.foodbase.FoodItem;
import org.bosik.compensation.persistence.common.MemoryBase;

public class FoodBaseXMLSerializerTest extends TestCase
{
	private final String				xml	= "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
													+ "<foods version=\"167\">\n"
													+ "	<food id=\"24785D5E44E14BF2917F3E3AAAA5C18E\" name=\"јбрикос\" prots=\"0.9\" fats=\"0.1\" carbs=\"9\" val=\"41\" table=\"True\"/>\n"
													+ "	<food id=\"5FB7320EF98C43E290F9DED3B8EBFC3A\" name=\"яйцо\" prots=\"12.7\" fats=\"11.5\" carbs=\"0.7\" val=\"157\" table=\"False\"/>\n"
													+ "</foods>";

	private final FoodBaseXMLSerializer	f	= new FoodBaseXMLSerializer();

	public void testRead()
	{
		MemoryBase<FoodItem> base = f.read(xml);

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

	public void testWriteRead()
	{
		MemoryBase<FoodItem> base = new MemoryBase<FoodItem>();
		FoodItem food;

		food = new FoodItem();
		food.setName("јбрикос");
		food.setRelProts(0.9);
		food.setRelFats(0.1);
		food.setRelCarbs(9.0);
		food.setRelValue(41);
		food.setFromTable(true);
		base.add(food);

		food = new FoodItem();
		food.setName("яйцо");
		food.setRelProts(12.7);
		food.setRelFats(11.5);
		food.setRelCarbs(0.7);
		food.setRelValue(157);
		food.setFromTable(false);
		base.add(food);

		assertEquals(2, base.count());
		assertEquals(2, base.getVersion());

		String xml = f.write(base);

		// =======================================================

		MemoryBase<FoodItem> anotherBase = f.read(xml);
		assertEquals(2, anotherBase.count());
		assertEquals(2, anotherBase.getVersion());

		for (int i = 0; i < base.count(); i++)
		{
			FoodItem food1 = base.get(i);
			FoodItem food2 = anotherBase.get(i);

			assertEquals(food1.getName(), food2.getName());
			assertEquals(food1.getRelProts(), food2.getRelProts());
			assertEquals(food1.getRelFats(), food2.getRelFats());
			assertEquals(food1.getRelCarbs(), food2.getRelCarbs());
			assertEquals(food1.getRelValue(), food2.getRelValue());
			assertEquals(food1.getFromTable(), food2.getFromTable());
		}
	}
}
