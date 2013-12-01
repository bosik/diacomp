package org.bosik.compensation.persistence.dao;

import junit.framework.ComparisonFailure;
import junit.framework.TestCase;
import org.bosik.compensation.bo.foodbase.FoodItem;
import android.util.Log;

public class FoodItemUtils extends TestCase
{
	private static final String	TAG	= FoodItemUtils.class.getSimpleName();
	private static final double	EPS	= 0.00001;

	public static String print(FoodItem item)
	{
		return String.format("%s: %s[%.1f|%.1f|%.1f|%.1f]:%s", item.getId(), item.getName(), item.getRelProts(),
				item.getRelFats(), item.getRelCarbs(), item.getRelValue(), item.getFromTable());
	}

	public static FoodItem demoFoodItemA()
	{
		FoodItem demo = new FoodItem();

		demo.setName("���� \"��������\" ������� (JUnit test)");
		demo.setId("270C3EC853464B3DB314067AD005A727");
		demo.setRelProts(5.5);
		demo.setRelFats(0.9);
		demo.setRelCarbs(44.1);
		demo.setRelValue(206.3);
		demo.setFromTable(false);

		return demo;
	}

	public static void compareItems(FoodItem exp, FoodItem act)
	{
		Log.i(TAG, "Comparison:");
		Log.d(TAG, print(exp));
		Log.d(TAG, print(act));

		try
		{
			// assertEquals(exp, act);
			assertEquals(exp.getName(), act.getName());
			assertEquals(exp.getId(), act.getId());
			assertEquals(exp.getRelProts(), act.getRelProts(), EPS);
			assertEquals(exp.getRelFats(), act.getRelFats(), EPS);
			assertEquals(exp.getRelCarbs(), act.getRelCarbs(), EPS);
			assertEquals(exp.getRelValue(), act.getRelValue(), EPS);
			assertEquals(exp.getFromTable(), act.getFromTable());
		}
		catch (ComparisonFailure e)
		{
			Log.e(TAG, "Comparison error:");
			Log.e(TAG, print(exp));
			Log.e(TAG, print(act));
			throw e;
		}
	}
}
