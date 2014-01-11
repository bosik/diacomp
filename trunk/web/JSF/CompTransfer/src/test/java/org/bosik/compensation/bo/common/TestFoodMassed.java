package org.bosik.compensation.bo.common;

import junit.framework.TestCase;
import org.bosik.compensation.bo.FoodMassed;

public class TestFoodMassed extends TestCase
{
	private FoodMassed	food	= new FoodMassed();

	// TODO: use EPS while comparing doubles
	// TODO: change Exception to IllegalArgumentException

	public void testMass()
	{
		// normal test
		food.setMass(0.0);
		assertEquals(0.0, food.getMass());
		food.setMass(0.01);
		assertEquals(0.01, food.getMass());
		food.setMass(100.0);
		assertEquals(100.0, food.getMass());
		food.setMass(999.5);
		assertEquals(999.5, food.getMass());

		// crash-test
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
		// normal test
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

		// crash-test
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
}
