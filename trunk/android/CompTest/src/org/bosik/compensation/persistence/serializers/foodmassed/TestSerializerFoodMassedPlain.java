package org.bosik.compensation.persistence.serializers.foodmassed;

import junit.framework.TestCase;
import org.bosik.diacomp.bo.FoodMassed;
import org.bosik.diacomp.persistence.serializers.SerializerFoodMassedPlain;

public class TestSerializerFoodMassedPlain extends TestCase
{
	private SerializerFoodMassedPlain	serializer	= new SerializerFoodMassedPlain();

	public void testRead()
	{
		// with dots
		FoodMassed food = serializer.read("Колбаса[12.7|19.1|0|270]:40");
		assertEquals("Колбаса", food.getName());
		assertEquals(12.7, food.getRelProts());
		assertEquals(19.1, food.getRelFats());
		assertEquals(0.0, food.getRelCarbs());
		assertEquals(270.0, food.getRelValue());
		assertEquals(40.0, food.getMass());

		// with both dots and commas
		food = serializer.read("Колбаса[12,7|19.1|0|270]:40");
		assertEquals("Колбаса", food.getName());
		assertEquals(12.7, food.getRelProts());
		assertEquals(19.1, food.getRelFats());
		assertEquals(0.0, food.getRelCarbs());
		assertEquals(270.0, food.getRelValue());
		assertEquals(40.0, food.getMass());
	}

	public void testReadAll()
	{
		// fail("Not yet implemented"); // TODO
	}

	public void testWrite()
	{
		FoodMassed food = new FoodMassed();
		food.setName("Колбаса");
		food.setRelProts(12.7);
		food.setRelFats(19.1);
		food.setRelCarbs(0);
		food.setRelValue(270);
		food.setMass(40);

		assertEquals("Колбаса[12,7|19,1|0|270]:40", serializer.write(food));
	}

	public void testWriteAll()
	{
		// fail("Not yet implemented"); // TODO
	}
}
