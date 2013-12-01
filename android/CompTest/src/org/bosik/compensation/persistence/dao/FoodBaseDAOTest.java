package org.bosik.compensation.persistence.dao;

import org.bosik.compensation.bo.foodbase.FoodItem;
import android.test.AndroidTestCase;

public abstract class FoodBaseDAOTest extends AndroidTestCase
{
	private BaseDAO<FoodItem>	foodBaseDAO;

	protected abstract BaseDAO<FoodItem> getDAO();

	@Override
	protected void setUp()
	{
		foodBaseDAO = getDAO();
	}

	public void testPersistanceSingle()
	{
		FoodItem org = new FoodItem();

		org.setName("Хлеб \"Бородино\" нарезка (JUnit test)");
		org.setId("270C3EC853464B3DB314067AD005A727");
		org.setRelProts(5.5);
		org.setRelFats(0.9);
		org.setRelCarbs(44.1);
		org.setRelValue(206.3);
		org.setFromTable(false);

		if (foodBaseDAO.findById(org.getId()) != null)
		{
			foodBaseDAO.delete(org.getId());
			assertNull(foodBaseDAO.findById(org.getId()));
		}

		foodBaseDAO.add(org);

		setUp();

		// ------------------------------

		FoodItem restored = foodBaseDAO.findById(org.getId());
		assertNotNull(restored);
		assertNotSame(org, restored);
		FoodItemUtils.compareItems(org, restored);

		// ------------------------------

		foodBaseDAO.delete(org.getId());
		assertNull(foodBaseDAO.findById(org.getId()));
	}
}
