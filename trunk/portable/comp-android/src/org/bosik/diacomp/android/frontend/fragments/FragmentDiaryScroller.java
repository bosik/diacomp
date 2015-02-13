package org.bosik.diacomp.android.frontend.fragments;

import java.util.Calendar;
import java.util.Date;
import org.bosik.diacomp.android.R;
import org.bosik.diacomp.android.frontend.views.diary.DiaryDayView;
import org.bosik.diacomp.core.utils.Utils;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ListView;

public class FragmentDiaryScroller extends Fragment
{
	private static final String	TAG	= FragmentDiaryScroller.class.getSimpleName();

	// Widgets
	private ListView			list;

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		setHasOptionsMenu(true);

		View rootView = inflater.inflate(R.layout.fragment_diary_scroller, container, false);

		Calendar c = Calendar.getInstance();
		c.set(Calendar.HOUR_OF_DAY, 0);
		c.set(Calendar.MINUTE, 0);
		c.set(Calendar.SECOND, 0);
		c.set(Calendar.MILLISECOND, 0);
		final Date baseDate = c.getTime();

		// Widgets binding
		list = (ListView) rootView.findViewById(R.id.listDiary);
		final BaseAdapter adapter = new BaseAdapter()
		{
			@Override
			public int getCount()
			{
				return Integer.MAX_VALUE;
			}

			@Override
			public Object getItem(int pos)
			{
				return pos;
			}

			@Override
			public long getItemId(int pos)
			{
				return pos;
			}

			@Override
			public View getView(int pos, View convertView, ViewGroup p)
			{
				final Date date = new Date(baseDate.getTime() + Utils.MsecPerDay * (pos - Integer.MAX_VALUE / 2));

				// if (convertView == null)
				{
					DiaryDayView diaryDayView = new DiaryDayView(FragmentDiaryScroller.this.getActivity(), date);
					return diaryDayView;
				}
				// else
				// {
				// DiaryDayView diaryDayView = (DiaryDayView) convertView;
				// diaryDayView.setDate(date);
				// return diaryDayView;
				// }
				// diaryDayView.setOnLoadedListener(new OnLoadedListener()
				// {
				// @Override
				// public void onLoaded(int contentHeight)
				// {
				// // FragmentDiaryScroller.this.list.scrollBy(0, -contentHeight);
				// Log.d(TAG, "Item at pos " + pos + " loaded with height " + contentHeight);
				// }
				// });

			}
		};
		list.setAdapter(adapter);
		list.setSelection(Integer.MAX_VALUE / 2);
		// list.setOnScrollListener(new OnScrollListener()
		// {
		// @Override
		// public void onScroll(AbsListView view, int firstVisibleItem, int visibleItemCount, int
		// totalItemCount)
		// {
		// // boolean loadMore = /* maybe add a padding */
		// // firstVisibleItem + visibleItemCount >= totalItemCount;
		// //
		// // if (loadMore)
		// // {
		// // count += visibleItemCount; // or any other amount
		// // adapter.notifyDataSetChanged();
		// // }
		// }
		//
		// @Override
		// public void onScrollStateChanged(AbsListView listView, int scrollState)
		// {
		// if (scrollState == SCROLL_STATE_IDLE)
		// {
		// int threshold = 5;
		// if (listView.getLastVisiblePosition() >= listView.getCount() - 1 - threshold)
		// {
		// // currentPage++;
		// // load more list items:
		// // loadElements(currentPage);
		// // count += 30;
		// // adapter.notifyDataSetChanged();
		// }
		// }
		// }
		// });

		return rootView;
	}
}