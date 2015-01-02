package org.bosik.diacomp.android.frontend.views.diary;

public interface RecordClickListener
{
	// THINK: что лучше передавать: индекс записи или её экземпляр?
	void onRecordClick(int index);
}
