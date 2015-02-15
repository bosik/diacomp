package org.bosik.diacomp.android.frontend.fragments;

import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.views.diary.DiaryDayView;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;

public class FragmentDiaryScroller extends Fragment
{
	private static final String	TAG	= FragmentDiaryScroller.class.getSimpleName();

	// Widgets
	private DiaryDayView		list;

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		setHasOptionsMenu(true);

		View rootView = inflater.inflate(R.layout.fragment_diary_scroller, container, false);

		// Widgets binding
		list = (DiaryDayView) rootView.findViewById(R.id.listDiary);
		// list.setDate(new Date());

		Button button = (Button) rootView.findViewById(R.id.button1);
		button.setOnClickListener(new OnClickListener()
		{
			@Override
			public void onClick(View v)
			{
			}
		});

		return rootView;
	}
}