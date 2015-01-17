package org.bosik.diacomp.core.services;

import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import junit.framework.TestCase;
import org.bosik.diacomp.core.entities.tech.Versioned;
import org.bosik.diacomp.core.services.ObjectService;
import org.bosik.diacomp.core.services.sync.SyncService;
import org.bosik.diacomp.core.test.fakes.services.FakeObjectService;
import org.junit.Test;

@SuppressWarnings({ "unchecked", "deprecation" })
public class TestSync extends TestCase
{
	private ObjectService<String>	service1;
	private ObjectService<String>	service2;

	private static final Date		firstDate	= new Date(0);

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

		Collections.sort(listExp, Versioned.COMPARATOR_GUID);
		Collections.sort(listAct, Versioned.COMPARATOR_GUID);

		for (int i = 0; i < listExp.size(); i++)
		{
			Versioned<String> exp = listExp.get(i);
			Versioned<String> act = listAct.get(i);
			assertEquals(exp, act);
		}
	}

	private void assertServicesAreSynced()
	{
		List<Versioned<String>> list1 = service1.findChanged(firstDate);
		List<Versioned<String>> list2 = service2.findChanged(firstDate);
		assertEquals(list1, list2);
	}

	@Override
	public void setUp()
	{
		service1 = new FakeObjectService();
		service2 = new FakeObjectService();
	}

	@Test
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
		assertServicesAreSynced();
		//		Versioned<String> restored = service2.findById(item.getId());
		//		assertEquals(item, restored);
	}

	@Test
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

		assertServicesAreSynced();
		//		Versioned<String> restored = service2.findById(item.getId());
		//		assertEquals(item, restored);
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

		// item check
		Versioned<String> restored = service2.findById(item.getId());
		assertNull(restored);

		// total check
		List<Versioned<String>> list2 = service2.findChanged(firstDate);
		assertTrue(list2.isEmpty());
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

		// total check
		assertServicesAreSynced();
	}

	public void test_sync_SingleCrossSync_SyncedOk()
	{
		// create item1 and save it in first storage

		Versioned<String> item1 = new Versioned<String>();
		item1.setData("Test 1");
		item1.setDeleted(false);
		item1.setId("a1b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8");
		item1.setTimeStamp(new Date(2014, 01, 01, 18, 30, 15));
		item1.setVersion(6);

		service1.save(Arrays.<Versioned<String>> asList(item1));

		// create item2 and save it in second storage

		Versioned<String> item2 = new Versioned<String>();
		item2.setData("Test 2");
		item2.setDeleted(true);
		item2.setId("b2c3d4e5f6d7c8a1b2c3d4e5f6d7c8a1");
		item2.setTimeStamp(new Date(2014, 01, 01, 19, 42, 57));
		item2.setVersion(2);
		service2.save(Arrays.<Versioned<String>> asList(item2));

		// sync
		SyncService.synchronize(service1, service2, new Date(2014, 01, 01, 00, 00, 00));

		// total check
		assertServicesAreSynced();
	}

	public void test_sync_ContrSync_SyncedOk()
	{
		// create items for first storage

		Date timeLess = new Date(2014, 01, 01, 10, 00, 00);
		Date timeSync = new Date(2014, 01, 01, 12, 00, 00);
		Date timeMore = new Date(2014, 01, 01, 14, 00, 00);

		Versioned<String> a1 = new Versioned<String>();
		a1.setData("a1 data");
		a1.setDeleted(false);
		a1.setId("a");
		a1.setTimeStamp(timeMore);
		a1.setVersion(5);

		Versioned<String> b1 = new Versioned<String>();
		b1.setData("b1 data");
		b1.setDeleted(false);
		b1.setId("b");
		b1.setTimeStamp(timeLess);
		b1.setVersion(20);

		service1.save(Arrays.<Versioned<String>> asList(a1, b1));

		// create items for second storage

		Versioned<String> a2 = new Versioned<String>();
		a2.setData("a2 data");
		a2.setDeleted(true);
		a2.setId("a");
		a2.setTimeStamp(timeLess);
		a2.setVersion(8);

		Versioned<String> b2 = new Versioned<String>();
		b2.setData("b2 data");
		b2.setDeleted(true);
		b2.setId("b");
		b2.setTimeStamp(timeMore);
		b2.setVersion(19);

		service2.save(Arrays.<Versioned<String>> asList(a2, b2));

		// sync
		SyncService.synchronize(service1, service2, timeSync);

		// total check
		assertServicesAreSynced();

		// accurate check
		assertEquals(a2, service1.findById("a"));
		assertEquals(a2, service2.findById("a"));
		assertEquals(b1, service1.findById("b"));
		assertEquals(b1, service2.findById("b"));
	}
}
