package org.bosik.compensation.face.views;

public interface RecordClickListener
{
	// TODO: что лучше передавать: индекс записи или её экземпляр?
	void onRecordClick(int index);
}
