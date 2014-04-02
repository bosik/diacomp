package org.bosik.diacomp.core.persistence.serializers.foodmassed;

import junit.framework.TestCase;
import org.bosik.diacomp.core.entities.business.FoodMassed;
import org.bosik.diacomp.core.persistence.serializers.SerializerFoodMassedPlain;

public class TestSerializerFoodMassedPlain extends TestCase
{
	private final SerializerFoodMassedPlain	serializer	= new SerializerFoodMassedPlain();

	public void test_read_normalDots_Ok()
	{
		// with dots
		FoodMassed food = serializer.read("Колбаса[12.7|19.1|0|270]:40");
		assertEquals("Колбаса", food.getName());
		assertEquals(12.7, food.getRelProts());
		assertEquals(19.1, food.getRelFats());
		assertEquals(0.0, food.getRelCarbs());
		assertEquals(270.0, food.getRelValue());
		assertEquals(40.0, food.getMass());
	}

	public void test_read_mixedDots_Ok()
	{
		// with both dots and commas
		FoodMassed food = serializer.read("Колбаса[12,7|19.1|0|270]:40");
		assertEquals("Колбаса", food.getName());
		assertEquals(12.7, food.getRelProts());
		assertEquals(19.1, food.getRelFats());
		assertEquals(0.0, food.getRelCarbs());
		assertEquals(270.0, food.getRelValue());
		assertEquals(40.0, food.getMass());
	}

	public void test_read_invalid_exceptionThrown()
	{
		try
		{
			serializer.read("$#^%#*&@");
			fail("Exception was not thrown");
		}
		catch (IllegalArgumentException e)
		{
			// just as planned
		}
	}

	public void test_write_normal_Ok()
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
}
