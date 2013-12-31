package org.bosik.compensation.fakes.mocks;

import org.bosik.compensation.bo.diary.DiaryRecord;
import org.bosik.compensation.persistence.common.Versioned;

// TODO: remove it
@Deprecated
public class MockDiaryPage // extends TestCase implements Mock<DiaryPage>
{
	private static final int							EPS_TIME					= 5 * 1000;				// msec
	private static final Mock<DiaryRecord>				mockDiaryRecord				= new MockDiaryRecord();
	private static final Mock<Versioned<DiaryRecord>>	mockDiaryRecordVersioned	= new MockVersionedConverter<DiaryRecord>(
																							mockDiaryRecord);

	// public List<DiaryPage> getSamples()
	// {
	// List<DiaryPage> samples = new ArrayList<DiaryPage>();
	//
	// DiaryPage page1 = new DiaryPage(new Date(2010, 10, 25), new Date(2010, 10, 25, 22, 15, 41),
	// 64);
	// for (DiaryRecord rec : mockDiaryRecord.getSamples())
	// {
	// page1.add(rec);
	// }
	// samples.add(page1);
	//
	// DiaryPage page2 = new DiaryPage(new Date(2010, 10, 26), new Date(2010, 10, 27, 00, 05, 59),
	// 48);
	// for (DiaryRecord rec : mockDiaryRecord.getSamples())
	// {
	// page2.add(rec);
	// page2.add(rec);
	// }
	// samples.add(page2);
	//
	// return samples;
	// }
	//
	// public void compare(DiaryPage exp, DiaryPage act)
	// {
	// // check the header
	// assertEquals(exp.getDate().getTime(), act.getDate().getTime(), EPS_TIME);
	// assertEquals(exp.getTimeStamp().getTime(), act.getTimeStamp().getTime(), EPS_TIME);
	// assertEquals(exp.getVersion(), act.getVersion());
	//
	// // check body
	// assertEquals(exp.count(), act.count());
	// for (int i = 0; i < exp.count(); i++)
	// {
	// mockDiaryRecordVersioned.compare(exp.get(i), act.get(i));
	// }
	// }
}
