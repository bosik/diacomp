package org.bosik.diacomp.android.face.views;

public interface RecordClickListener
{
	// THINK: что лучше передавать: индекс записи или её экземпляр?
	void onRecordClick(int index);
}
