package org.bosik.diacomp.web.frontend.wicket.dialogs.common;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.modal.ModalWindow;
import org.apache.wicket.model.Model;
import org.apache.wicket.util.string.AppendingStringBuffer;
import org.bosik.diacomp.core.entities.tech.Versioned;

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
		// FIXME: not working
		if (!isShown())
		{
			final AppendingStringBuffer buffer = new AppendingStringBuffer(500);
			buffer.append("function mwClose(ev) {\n" + "var code = ev.keyCode || ev.which;\n"
					+ "if (code == 27) { "
					+ getCloseJavacript() + "};" + "}");

			buffer.append("jQuery(document).keypress(mwClose);\n");
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

	public abstract void onSave(AjaxRequestTarget target, Model<Versioned<T>> model);
}
