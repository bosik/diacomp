package org.bosik.compensation.bo.common;

import java.text.ParseException;
import org.bosik.compensation.bo.common.FoodMassed;
import junit.framework.TestCase;

public class FoodMassedTest extends TestCase
{
	private FoodMassed	food	= new FoodMassed();

	public void testMass()
	{
		// нормальный тест
		food.setMass(0.0);
		assertEquals(0.0, food.getMass());
		food.setMass(0.01);
		assertEquals(0.01, food.getMass());
		food.setMass(100.0);
		assertEquals(100.0, food.getMass());
		food.setMass(999.5);
		assertEquals(999.5, food.getMass());

		// краш-тест
		try
		{
			food.setMass(-0.01);
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			food.setMass(-100);
			fail();
		}
		catch (Exception e)
		{
		}
	}

	public void testGetAbs()
	{
		// нормальный тест
		food.setMass(10);

		food.setRelProts(1);
		assertEquals(0.1, food.getProts());
		food.setRelFats(2);
		assertEquals(0.2, food.getFats());
		food.setRelCarbs(3);
		assertEquals(0.3, food.getCarbs());
		food.setRelValue(4);
		assertEquals(0.4, food.getValue());

		food.setMass(1000);

		food.setRelProts(2);
		assertEquals(20.0, food.getProts());
		food.setRelFats(4);
		assertEquals(40.0, food.getFats());
		food.setRelCarbs(6);
		assertEquals(60.0, food.getCarbs());
		food.setRelValue(8);
		assertEquals(80.0, food.getValue());

		food.setMass(0);

		assertEquals(0.0, food.getProts());
		assertEquals(0.0, food.getFats());
		assertEquals(0.0, food.getCarbs());
		assertEquals(0.0, food.getValue());

		// краш-тест
		try
		{
			food.setRelProts(-0.01);
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			food.setRelProts(100.01);
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			food.setRelFats(-0.01);
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			food.setRelFats(100.01);
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			food.setRelCarbs(-0.01);
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			food.setRelCarbs(100.01);
			fail();
		}
		catch (Exception e)
		{
		}
		try
		{
			food.setRelValue(-0.01);
			fail();
		}
		catch (Exception e)
		{
		}
	}

	public void testRead()
	{
		try
		{
			// с точками
			food.read("Колбаса[12.7|19.1|0|270]:40");
			assertEquals("Колбаса", food.getName());
			assertEquals(12.7, food.getRelProts());
			assertEquals(19.1, food.getRelFats());
			assertEquals(0.0, food.getRelCarbs());
			assertEquals(270.0, food.getRelValue());
			assertEquals(40.0, food.getMass());

			// с запятыми и точками
			food.read("Колбаса[12,7|19.1|0|270]:40");
			assertEquals("Колбаса", food.getName());
			assertEquals(12.7, food.getRelProts());
			assertEquals(19.1, food.getRelFats());
			assertEquals(0.0, food.getRelCarbs());
			assertEquals(270.0, food.getRelValue());
			assertEquals(40.0, food.getMass());

		}
		catch (ParseException e)
		{
			fail("Parse exception");
		}
	}

	public void testWrite()
	{
		food.setName("Колбаса");
		food.setRelProts(12.7);
		food.setRelFats(19.1);
		food.setRelCarbs(0);
		food.setRelValue(270);
		food.setMass(40);

		assertEquals("Колбаса[12,7|19,1|0|270]:40", food.write());
	}

	public void testClone() throws CloneNotSupportedException
	{
		food.setName("Колбаса");
		food.setRelProts(12.7);
		food.setRelFats(19.1);
		food.setRelCarbs(0.1);
		food.setRelValue(270);
		food.setMass(40);

		FoodMassed copy = (FoodMassed) food.clone();
		assertEquals(copy, food);
		assertEquals(copy.getId(), food.getId());
		assertEquals(copy.write(), food.write());
	}
}
