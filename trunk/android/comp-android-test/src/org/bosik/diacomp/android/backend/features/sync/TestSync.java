package org.bosik.diacomp.android.backend.features.sync;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import junit.framework.TestCase;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.ObjectService;

@SuppressWarnings({ "unchecked", "deprecation" })
public class TestSync extends TestCase
{
	private ObjectService<String>	service1;
	private ObjectService<String>	service2;

	private void assertEquals(Versioned<String> exp, Versioned<String> act)
	{
		assertNotNull(exp);
		assertNotNull(act);

		assertEquals(exp.getId(), act.getId());
		assertEquals(exp.getVersion(), act.getVersion());
		assertEquals(exp.getTimeStamp(), act.getTimeStamp());
		assertEquals(exp.isDeleted(), act.isDeleted());

		assertEquals(exp.getData(), act.getData());
	}

	private void assertEquals(List<Versioned<String>> listExp, List<Versioned<String>> listAct)
	{
		assertNotNull(listExp);
		assertNotNull(listAct);
		assertEquals(listExp.size(), listAct.size());

		for (int i = 0; i < listExp.size(); i++)
		{
			Versioned<String> exp = listExp.get(i);
			Versioned<String> act = listAct.get(i);
			assertEquals(exp, act);
		}
	}

	@Override
	public void setUp()
	{
		service1 = new FakeObjectService();
		service2 = new FakeObjectService();
	}

	public void test_sync_SingleAdd_SyncedOk()
	{
		Versioned<String> item = new Versioned<String>();
		item.setData("Test");
		item.setDeleted(true);
		item.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item.setVersion(42);

		service1.save(Arrays.<Versioned<String>> asList(item));

		SyncService.synchronize(service1, service2, new Date(2014, 01, 01, 18, 30, 00));

		Versioned<String> restored = service2.findById(item.getId());
		assertEquals(item, restored);
	}

	public void test_sync_SingleAddReverse_SyncedOk()
	{
		Versioned<String> item = new Versioned<String>();
		item.setData("Test");
		item.setDeleted(true);
		item.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item.setVersion(42);

		service1.save(Arrays.<Versioned<String>> asList(item));

		SyncService.synchronize(service2, service1, new Date(2014, 01, 01, 18, 30, 00));

		Versioned<String> restored = service2.findById(item.getId());
		assertEquals(item, restored);
	}

	public void test_sync_SingleOldAdd_NotSynced()
	{
		Versioned<String> item = new Versioned<String>();
		item.setData("Test");
		item.setDeleted(true);
		item.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item.setVersion(42);

		service1.save(Arrays.<Versioned<String>> asList(item));

		SyncService.synchronize(service1, service2, new Date(2014, 01, 01, 18, 30, 16));

		Versioned<String> restored = service2.findById(item.getId());
		assertEquals(item, restored);
	}

	public void test_sync_SingleChanged_SyncedOk()
	{
		// create item and save it in both sources

		Versioned<String> item = new Versioned<String>();
		item.setData("Test");
		item.setDeleted(false);
		item.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item.setVersion(42);

		service1.save(Arrays.<Versioned<String>> asList(item));
		service2.save(Arrays.<Versioned<String>> asList(item));

		// modify item
		item.setData("Updated");
		item.setDeleted(true);
		item.setTimeStamp(new Date(2014, 01, 01, 19, 00, 00));
		item.setVersion(43);

		// save it only in first source
		service1.save(Arrays.<Versioned<String>> asList(item));

		// sync
		SyncService.synchronize(service1, service2, new Date(2014, 01, 01, 18, 30, 00));

		// check the result
		assertEquals(item, service2.findById(item.getId()));
	}
}
