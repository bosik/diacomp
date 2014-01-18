package org.bosik.compensation.persistence.dao;

import org.bosik.compensation.fakes.mocks.Mock;
import org.bosik.compensation.fakes.mocks.MockFoodItem;
import org.bosik.compensation.fakes.mocks.MockVersionedConverter;
import org.bosik.diacomp.bo.foodbase.FoodItem;
import org.bosik.diacomp.persistence.common.Versioned;
import org.bosik.diacomp.persistence.dao.FoodBaseDAO;
import org.junit.Assert;

public abstract class TestFoodBaseDAO
{
	private FoodBaseDAO							foodBaseDAO_cached;
	private static Mock<Versioned<FoodItem>>	mockGenerator	= new MockVersionedConverter<FoodItem>(
																		new MockFoodItem());

	protected abstract FoodBaseDAO getDAO();

	private FoodBaseDAO getFoodBaseDAO()
	{
		if (foodBaseDAO_cached == null)
		{
			foodBaseDAO_cached = getDAO();
		}
		return foodBaseDAO_cached;
	}

	public void testPersistanceSingle()
	{
		Versioned<FoodItem> org = mockGenerator.getSamples().get(0);
		// int version;

		if (getFoodBaseDAO().findById(org.getId()) != null)
		{
			// version = foodBaseDAO.getVersion();
			getFoodBaseDAO().delete(org.getId());
			// assertEquals(version + 1, foodBaseDAO.getVersion());
			Assert.assertNull(getFoodBaseDAO().findById(org.getId()));
		}

		// version = foodBaseDAO.getVersion();
		getFoodBaseDAO().add(org);
		// assertEquals(version + 1, foodBaseDAO.getVersion());

		// ------------------------------
		foodBaseDAO_cached = null; // reset
		// ------------------------------

		Versioned<FoodItem> restored = getFoodBaseDAO().findById(org.getId());
		Assert.assertNotNull(restored);
		Assert.assertNotSame(org, restored);
		mockGenerator.compare(org, restored);

		// version = foodBaseDAO.getVersion();
		getFoodBaseDAO().delete(org.getId());
		// assertEquals(version + 1, foodBaseDAO.getVersion());
		Assert.assertNull(getFoodBaseDAO().findById(org.getId()));
	}

	// TODO: create testPersistenceMultiple()

	public void testPersistanceMultiple()
	{

	}
}
