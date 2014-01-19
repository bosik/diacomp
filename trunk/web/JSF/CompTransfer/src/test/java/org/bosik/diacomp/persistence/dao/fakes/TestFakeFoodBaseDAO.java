package org.bosik.diacomp.persistence.dao.fakes;

import org.bosik.diacomp.fakes.dao.FakeFoodBaseDAO;
import org.bosik.diacomp.persistence.dao.FoodBaseDAO;
import org.bosik.diacomp.persistence.dao.TestFoodBaseDAO;
import org.junit.Ignore;

@Ignore
public class TestFakeFoodBaseDAO extends TestFoodBaseDAO
{
	@Override
	protected FoodBaseDAO getDAO()
	{
		return new FakeFoodBaseDAO();
	}
}