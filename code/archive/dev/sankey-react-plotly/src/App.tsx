import { Sankey } from "./Sankey";

export const App = () => {
  const handleNavigate = (target: string) => {
    console.log("navigate ->", target);
    alert(`Navigate to: ${target}`);
  };

  return (
    <div className="page">
      <h2 className="title">Jury Pipeline (plotly)</h2>
      <div className="sankey-wrap">
        <Sankey onNavigate={handleNavigate} />
      </div>
    </div>
  );
};
