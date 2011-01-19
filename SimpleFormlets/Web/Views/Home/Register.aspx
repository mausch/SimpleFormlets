<%@ Page Title="" Language="C#" MasterPageFile="~/Views/Shared/Site.Master" Inherits="System.Web.Mvc.ViewPage" %>

<asp:Content ID="Content1" ContentPlaceHolderID="TitleContent" runat="server">
	Register
</asp:Content>

<asp:Content ID="Content2" ContentPlaceHolderID="MainContent" runat="server">

    <%: ViewData["Ordered"] %>: <%: ViewData["Name"] %> has requested <%: ViewData["Amount"] %> items to be delivered on <%: ViewData["Shipping"] %>

</asp:Content>
