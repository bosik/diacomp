/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.frontend.wicket.dialogs.common;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.modal.ModalWindow;
import org.apache.wicket.model.IModel;
import org.apache.wicket.util.string.AppendingStringBuffer;
import org.bosik.merklesync.Versioned;

public abstract class CommonEditor<T> extends ModalWindow
{
	private static final long	serialVersionUID	= 1L;

	public CommonEditor(String id)
	{
		super(id);
		setCssClassName(CSS_CLASS_GRAY);
	}

	@Override
	public void show(final AjaxRequestTarget target)
	{
		if (!isShown())
		{
			final AppendingStringBuffer buffer = new AppendingStringBuffer(500);
			buffer.append("function mwClose(ev) {\n" + "var code = ev.keyCode || ev.which;\n" + "if (code == 27) { "
					+ getCloseJavacript() + "};" + "}");

			buffer.append("jQuery(document).keyup(mwClose);\n");
			target.appendJavaScript(buffer.toString());
		}

		super.show(target);
	}

	//		@Override
	//		public void renderHead(IHeaderResponse response)
	//		{
	//			super.renderHead(response);
	//			response.renderOnDomReadyJavaScript(" if($(document).data('wicketWindowCloseBound')) {return;} "
	//					+ " $(document).data('wicketWindowCloseBound', true); "
	//					+ " $(document).bind('keyup', function(evt) {\n" + "    if (evt.keyCode == 27) {\n"
	//					+ getCloseJavacript() + "\n" + "        evt.preventDefault();\n" + "        evt.stopPropagation();\n"
	//					+ "    }\n" + "  });\n");
	//		}

	//	private static String getCloseJavacript() {
	//        return "var win;\n" //
	//            + "try {\n"
	//            + " win = window.parent.Wicket.Window;\n"
	//            + "} catch (ignore) {\n"
	//            + "}\n"
	//            + "if (typeof(win) == \"undefined\" || typeof(win.current) == \"undefined\") {\n"
	//            + "  try {\n"
	//            + "     win = window.Wicket.Window;\n"
	//            + "  } catch (ignore) {\n"
	//            + "  }\n"
	//            + "}\n"
	//            + "if (typeof(win) != \"undefined\" && typeof(win.current) != \"undefined\") {\n"
	//            + " var close = function(w) { w.setTimeout(function() {\n"
	//            + "     win.current.close();\n"
	//            + " }, 0);  } \n"
	//            + " try { close(window.parent); } catch (ignore) { close(window); };\n" + "}";
	//    }

	public abstract void onCancel(AjaxRequestTarget target);

	public abstract void onSave(AjaxRequestTarget target, IModel<Versioned<T>> model);
}
