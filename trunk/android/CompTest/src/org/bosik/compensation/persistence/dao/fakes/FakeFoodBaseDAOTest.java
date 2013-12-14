package org.bosik.compensation.persistence.dao.fakes;

import org.bosik.compensation.fakes.dao.FakeFoodBaseDAO;
import org.bosik.compensation.persistence.dao.FoodBaseDAO;
import org.bosik.compensation.persistence.dao.FoodBaseDAOTest;
import android.test.suitebuilder.annotation.Suppress;

@Suppress
public class FakeFoodBaseDAOTest extends FoodBaseDAOTest
{
	@Override
	protected FoodBaseDAO getDAO()
	{
		return new FakeFoodBaseDAO();
	}
}